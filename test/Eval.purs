-- | A fake sort-of evaluator for Erlang, to test that optimizations are safe.
-- | It traces control flow and "values" (not fully evaluated to be sure).
-- |
-- | In particular, it elides local variables to get expressions in terms of
-- | deBruijn indices for function arguments. It traces function calls in
-- | assignments to make sure that discarded calls (especially effectful ones)
-- | do not get neglected.
-- |
-- | Makes a lot of assumptions to get by (in particular, that before and after
-- | share similar code structure). It also borrows some functions directly from
-- | the optimizations (`optimizeIf`, `floating`, `globalThunk`), so they should
-- | be audited separately from this evaluator. But this module generally serves
-- | as specification for those optimizations (sorry for writing inscrutible code).
-- |
-- | This could be plugged into a QuickCheck framework.
module Test.Eval where

import Prelude

import Control.Monad.Reader (ReaderT(..), asks, lift, local, runReaderT)
import Control.Monad.State (StateT(..), evalStateT, modify)
import Data.Array (intercalate)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bitraversable (bitraverse)
import Data.Filterable (filterMap)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Identity (Identity)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe, maybe')
import Data.Semigroup.Foldable (intercalateMap)
import Data.Traversable (class Foldable, class Traversable, all, fold, foldMap, foldr, traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Dodo as D
import Effect.Aff (Aff, catchError)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Library.Execa (execa)
import Partial.Unsafe (unsafeCrashWith)
import PureScript.Backend.Erl.Convert.After (globalThunk, optimizeIf)
import PureScript.Backend.Erl.Convert.Scoping (floating)
import PureScript.Backend.Erl.Printer as P
import PureScript.Backend.Erl.Syntax (Accessor(..), CaseClause(..), ErlDefinition(..), ErlExpr(..), ErlLiteral(..), ErlModule, ErlPattern(..), FunHead(..), Guard(..), IfClause(..), access, applyAccessors')
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Unsafe.Reference (unsafeRefEq)


data TraceCall = TraceCall (Maybe ErlExpr) ErlExpr (Array ErlExpr)
derive instance eqTraceCall :: Eq TraceCall

type Scope = Map String ErlExpr
type VarsArgs =
  { vars :: Scope
  , args :: Int
  , tracing :: Boolean
  }


type Traced = Control ErlExpr

type TraceT :: (Type -> Type) -> Type -> Type
type TraceT m = StateT Int (ReaderT VarsArgs m)
type Trace = TraceT Identity
type Tracing = TraceT Control ErlExpr

liftCtl :: Control ~> TraceT Control
liftCtl = lift <<< lift

type ACase u v =
  { match :: Array ErlPattern
  , guard :: Maybe u
  , branch :: v
  }

-- Free monad capturing control flow
data Control a
  = Return a
  -- A case expression inspecting some expressions, matching/guarding some
  -- branches of control flow
  | CaseCtl (Array ErlExpr) (NonEmptyArray (ACase (Control Guard) (Control a)))
  -- Function with its designated number (in order of appearance),
  -- designated arguments (deBruijn index), definition, and the reference
  -- (/return value/continuation),
  | FunCtl Int (Array Int) Traced (Control a)
  -- Trace a call. This is important for effectful actions.
  | TraceCtl TraceCall (Control a)

derive instance eqControl :: Eq a => Eq (Control a)

returned :: forall a. Control a -> a
returned (Return a) = a
returned (CaseCtl _ cases) = returned (NEA.last cases).branch
returned (FunCtl _ _ _ c) = returned c
returned (TraceCtl _ c) = returned c

derive instance functorControl :: Functor Control
derive instance foldableControl :: Foldable Control
derive instance traversableControl :: Traversable Control
instance applyControl :: Apply Control where
  apply = ap
instance applicativeControl :: Applicative Control where
  pure = Return
instance bindControl :: Bind Control where
  bind (Return a) f = f a
  bind (CaseCtl inspect cases) f = CaseCtl inspect $
    cases <#> \r ->
      { match: r.match
      , guard: r.guard
      , branch: r.branch >>= f
      }
  bind (FunCtl i a d c) f = FunCtl i a d (c >>= f)
  bind (TraceCtl i c) f = TraceCtl i (c >>= f)
instance monadControl :: Monad Control

disableTrace :: TraceT Control ~> TraceT Control
disableTrace = local $ prop (Proxy :: Proxy "tracing") $ const false

enableTrace :: TraceT Control ~> TraceT Control
enableTrace = local $ prop (Proxy :: Proxy "tracing") $ const true

-- Beta-reduce expressions like `(fun (arg1,arg2) -> ... end)(arg1,arg2,...)`.
-- To apply to curried functions, this needs to apply bottom-up, but `eval`
-- works top-down, so instead we iterate it.
applyFunCalls :: ErlExpr -> Maybe ErlExpr
applyFunCalls (FunCall Nothing fun args) =
  case floating fun of
    Tuple floated (FunName qual val arity) | arity == Array.length args ->
      Just $ floated $ FunCall qual val args
    Tuple floated (Fun Nothing [Tuple (FunHead pats Nothing) body])
      | Array.length pats == Array.length args ->
        Just $ floated <<< Assignments (Array.zip pats args) $ body
    Tuple floated1 s'@(FunCall Nothing _ _)
      | Just s'' <- applyFunCalls s'
      , Tuple floated2 (Fun Nothing [Tuple (FunHead pats Nothing) body]) <- floating s''
      , Array.length pats == Array.length args ->
        Just $ floated1 <<< floated2 <<< Assignments (Array.zip pats args) $ body
    _ -> Nothing
applyFunCalls _ = Nothing

eval :: ErlExpr -> Tracing
eval = case _ of
  -- Humdrum values
  Literal l -> pure (Literal l)
  List items -> List <$> traverse eval items
  ListCons items tail ->
    ListCons <$> traverse eval items <*> eval tail
  Tupled items -> Tupled <$> traverse eval items
  Map kvs -> Map <$> traverse (bitraverse eval eval) kvs
  MapUpdate base kvs ->
    MapUpdate <$> eval base <*> traverse (bitraverse eval eval) kvs
  Record kvs -> Record <$> traverse (traverse eval) kvs
  RecordUpdate base kvs ->
    RecordUpdate <$> eval base <*> traverse (traverse eval) kvs
  BinOp op l r -> BinOp op <$> eval l <*> eval r
  UnaryOp op e -> UnaryOp op <$> eval e
  BinaryAppend l r -> BinaryAppend <$> eval l <*> eval r

  -- Handle variables
  Var v acsrs -> do
    asks (_.vars >>> Map.lookup v) <#> maybe
      -- not found? return old value unchanged
      (Var v acsrs)
      -- found: apply accessors to the new value
      (applyAccessors' true <@> acsrs)

  -- Assignments are the main place where we need to trace function calls,
  -- because we want to verify that the calls are identical and we do not know
  -- if the assignment will show up elsewhere (effectful action that is
  -- discarded? variable that does not get used?).
  Assignments asgns body ->
    let
      f (Tuple pat expr) next = do
        evaled <- enableTrace (eval expr)
        matching pat evaled next
    in foldr f (eval body) asgns

  Macro name margs ->
    -- Enable tracing since variables get hoisted out of macros into assignments
    enableTrace $ Macro name <$> traverse (traverse eval) margs

  -- We never trace calls like `module_name@ps:topLevel()`, since those are pure
  s@(FunCall _ _ _) | Just _ <- globalThunk s -> pure s
  -- We apply immediate function calls if possible
  s@(FunCall Nothing _ _) | Just s' <- applyFunCalls s -> eval s'
  -- Otherwise we evaluate the arguments and trace it if in tracing mode
  FunCall mod name args -> do
    -- Disable trace while evaluating arguments, since we know their result
    -- is recorded here.
    modE <- disableTrace $ traverse eval mod
    nameE <- disableTrace $ eval name
    argsE <- disableTrace $ traverse eval args
    whenM (asks _.tracing) do
      liftCtl $ TraceCtl
        (TraceCall modE nameE argsE)
        (Return unit)
    pure (FunCall modE nameE argsE)

  -- Evaluating a function captures the control flow inside it as the type
  -- `Traced = Control ErlExpr` and returns a fresh number for the function
  Fun name cases ->
    case NEA.fromArray cases, funArity (map fst cases) of
      Nothing, _ -> unsafeCrashWith "empty function"
      Just casez, arity -> funCtl name arity do
        base <- asks _.args
        let inspect = Array.replicate arity unit # mapWithIndex \i _ -> Var ("$" <> show (base - arity + i)) []
        caseEvals inspect $ casez <#> \(Tuple (FunHead match mg) branch) ->
          { match: match
          , guard: coerce mg
          , branch
          }

  FunName me e arity -> FunName <$> traverse eval me <*> eval e <@> arity

  -- We cheat and use the same `optimizeIf` here to make sure that control flow
  -- matches
  If clauses ->
    case optimizeIf clauses of
      If clauses' -> caseEvals [] $ clauses' <#>
        \(IfClause cond branch) ->
          { match: []
          , guard: Just cond
          , branch
          }
      s -> eval s
  -- Control flow from case branches
  Case inspect cases -> caseEvals [inspect] $ cases <#>
    \(CaseClause match mg branch) ->
      { match: [match]
      , guard: mg
      , branch
      }

funArity :: Array FunHead -> Int
funArity funHeads =
  case Array.nub $ funHeads <#> \(FunHead pats _) -> Array.length pats of
    [ arity ] -> arity
    [] -> unsafeCrashWith "empty function"
    _ -> unsafeCrashWith "inconsistent function arity"

funCtl :: Maybe String -> Int -> Tracing -> Tracing
funCtl name i x = do
  desig <- (_ - 1) <$> modify (add 1)
  reference <- pure $ Var ("#" <> show desig) []
  definition <- controlled do
    flip local x $
      prop (Proxy :: Proxy "args") (add i) >>>
      prop (Proxy :: Proxy "vars") (maybe identity (flip Map.insert (reference)) name)
  args <- asks \{ args } -> Array.replicate i unit # mapWithIndex \j _ -> args + j
  liftCtl $ FunCtl desig args definition (Return reference)

caseEvals ::
  Array ErlExpr ->
  NonEmptyArray (ACase Guard ErlExpr) ->
  Tracing
caseEvals inspect x = do
  i <- traverse (disableTrace <<< eval) inspect
  y <- traverse (f i) (trim x)
  casing' i y
  where
  flt (FunCall (Just (Literal (Atom "erlang"))) (Literal (Atom "error")) [Tupled [ Literal (Atom "fail"), Literal (String _i) ]]) = false
  flt _ = true
  trim = NEA.filter (_.branch >>> flt) >>> NEA.fromArray >>> maybe' (\_ -> unsafeCrashWith "no branches") identity
  f i r = ado
    g <- traverse (controlled <<< coerce eval) r.guard
    b <- matchingAll (Array.zip r.match i) $ controlled (eval r.branch)
    in { match: simplify <$> r.match, guard: g, branch: b }

casing' :: forall a.
  Array ErlExpr ->
  NonEmptyArray (ACase (Control Guard) (Control a)) ->
  TraceT Control a
casing' _inspect cases
  | { match, guard: Nothing, branch } <- NEA.head cases
  , match # all (eq Discard) = liftCtl branch
casing' _inspect cases | NEA.length cases == 1 = liftCtl (NEA.head cases).branch
casing' inspect cases = liftCtl (CaseCtl inspect cases)

controlled :: forall a. TraceT Control a -> TraceT Control (Control a)
controlled (StateT f1) =
  StateT \s ->
    ReaderT \r ->
      case f1 s of
        ReaderT f2 ->
          case f2 r of
            ctl -> pure $ Tuple (fst <$> ctl) (snd (returned ctl))

-- Create a scope by matching a pattern. This turns pattern matches back into
-- access operations on the values.
matchVars :: ErlPattern -> ErlExpr -> Scope
matchVars = case _ of
  Discard -> const Map.empty
  MatchLiteral _lit -> const Map.empty
  BindVar v -> \e -> Map.singleton v e
  MatchBoth v pat -> \e -> Map.insert v e $ matchVars pat e
  MatchMap pats -> \e -> Map.unions $ pats <#>
    \(Tuple key pat) -> matchVars pat (access (AcsKey key) e)
  MatchTuple pats -> \e -> Map.unions $ pats # mapWithIndex
    \i pat -> matchVars pat (access (AcsElement (i+1)) e)
  MatchList pats tail -> \e -> Map.unions do
    let
      head = pats # mapWithIndex
        \i pat -> matchVars pat (access (AcsItem i) e)
      addTail = maybe identity (flip Array.snoc) $
        matchVars <$> tail <@> access (AcsDrop (Array.length pats)) e
    addTail head

matching :: ErlPattern -> ErlExpr -> TraceT Control ~> TraceT Control
matching pat val = local $ prop (Proxy :: Proxy "vars") $ Map.union $ matchVars pat val

matchingAll :: Array (Tuple ErlPattern ErlExpr) -> TraceT Control ~> TraceT Control
matchingAll patsvals =
  local $ prop (Proxy :: Proxy "vars") $ Map.union $ Map.unions $
    map (uncurry matchVars) patsvals

-- Drop variable names, and even drop redundant `MatchMap`s, since the typing
-- rules of PureScript ensure they should always match.
simplify :: ErlPattern -> ErlPattern
simplify = case _ of
  Discard -> Discard
  MatchLiteral lit -> MatchLiteral lit
  BindVar _ -> Discard
  MatchBoth _ pat -> simplify pat
  MatchMap pats -> case map simplify <$> pats of
    pats' | all (all (eq Discard)) pats' -> Discard
    pats' -> MatchMap pats'
  MatchTuple pats -> MatchTuple $ simplify <$> pats
  MatchList pats tail -> MatchList (simplify <$> pats) (simplify <$> tail)


run :: ErlExpr -> Traced
run = eval
  >>> flip evalStateT 0
  >>> flip runReaderT { args: 0, vars: Map.empty, tracing: true }

-- A printer for debugging
print :: Traced -> D.Doc Void
print (Return e) = D.text "“" <> P.printExpr e <> D.text "”"
print (TraceCtl (TraceCall x y z) r@(Return (FunCall x' y' z')))
  | unsafeRefEq x x' && unsafeRefEq y y' && unsafeRefEq z z' =
    print r
print (TraceCtl (TraceCall x y z) r) = fold
  [ D.text "¡ "
  , D.indent $ P.printExpr (FunCall x y z)
  , D.break
  , print r
  ]
print (FunCtl desig0 args0 ctl0 r0) = go [args0] ctl0
  where
  go acc (FunCtl desig' args' ctl' (Return (Var desig'' [])))
    | desig'' == "#" <> show desig' =
      go (Array.snoc acc args') ctl'
  go acc ctl = fold
    [ D.text $ "\\#" <> show desig0
    , D.text $ acc # foldMap \args ->
        "(" <> intercalate "," (map (\a -> "$" <> show a) args) <> ")"
    , D.text ":"
    , D.break
    , D.indent (print ctl)
    , D.break
    , case r0 of
        Return (Var "#0" []) -> mempty
        _ -> print r0
    ]
print (CaseCtl _ cases)
  | { match, guard: Nothing, branch } <- NEA.head cases
  , match # all (eq Discard) = print branch
print (CaseCtl [] cases) = cases # intercalateMap D.break \{ guard, branch } -> fold
  [ D.text "? " <> D.indent (print (fromMaybe (pure (Literal (Atom "true"))) (coerce guard)))
  , D.text ":" <> D.break
  , D.indent $ print branch
  ]
print (CaseCtl inspect cases) = fold
  [ D.text "¿ "
  , intercalate (D.break <> D.text "¿ ") $ D.indent <<< P.printExpr <$> inspect
  , cases # foldMap \{ match, guard, branch } -> fold
    [ D.break <> D.text "@ "
    , intercalate (D.text ", ") $ P.printPattern <$> match
    , P.sepMaybe (D.text " when ") print (coerce guard)
    , D.text ":" <> D.break
    , D.indent $ print branch
    ]
  ]

-- Look at a module and determine if `rawDefinitions` and `definitions` (before
-- and after optimizations) will evaluate the same way at runtime.
--
-- Only runs if `./debug` exists and will write files there if they differ.
-- Not very efficient and will run into bad complexity on some files.
printModule :: ErlModule -> Aff Unit
printModule { moduleName, rawDefinitions, definitions } = flip catchError mempty do
  _ <- FS.stat "./debug"
  let
    rawPath = ("./debug/raw-" <> moduleName <> ".purs-erl-trace")
    rawDefs = Map.fromFoldable $ rawDefinitions <#> \(FunctionDefinition name args body) ->
      Tuple (Tuple name (Array.length args)) $
        Fun Nothing [ Tuple (FunHead args Nothing) body ]
    optPath = ("./debug/opt-" <> moduleName <> ".purs-erl-trace")
    optDefs = Map.fromFoldable $ definitions <#> \(FunctionDefinition name args body) ->
      Tuple (Tuple name (Array.length args)) $
        Fun Nothing [ Tuple (FunHead args Nothing) body ]
    diffPath = "./debug/" <> moduleName <> ".purs-erl-trace.diff"
    diffEval = (Tuple <$> rawDefs <*> optDefs) # filterMap \(Tuple rawExpr optExpr) -> do
      let
        raw = run rawExpr
        opt = run optExpr
      if raw /= opt then Just (Tuple raw opt) else Nothing
    rawEval = fst <$> diffEval
    optEval = snd <$> diffEval
  if rawEval /= optEval then do
    FS.writeTextFile UTF8 rawPath $
      D.print D.plainText D.twoSpaces $
        rawEval # foldMapWithIndex \(Tuple name _) expr -> fold
          [ D.text name
          , print expr
          , D.break
          ]
    FS.writeTextFile UTF8 optPath $
      D.print D.plainText D.twoSpaces $
        optEval # foldMapWithIndex \(Tuple name _) expr -> fold
          [ D.text name
          , print expr
          , D.break
          ]
    void $ execa "git"
      [ "diff", "--no-index"
      , "--output=" <> diffPath
      , rawPath
      , optPath
      ] identity >>= _.getResult
  else do
    FS.rm' rawPath { recursive: false, force: true, maxRetries: 0, retryDelay: 100 }
    FS.rm' optPath { recursive: false, force: true, maxRetries: 0, retryDelay: 100 }
    FS.rm' diffPath { recursive: false, force: true, maxRetries: 0, retryDelay: 100 }
