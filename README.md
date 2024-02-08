# `purescript-backend-erl`

_PureScript to Erlang optimizing compiler, based on [`backend-optimizer`](https://github.com/aristanetworks/purescript-backend-optimizer)._

`purescript-backend-erl` aims to be source-compatible with [`purerl`](https://github.com/purerl/purerl) code.
In particular, the representation of datatypes remains unchanged and existing FFI files should work as-is.
(The generated Erlang files will not be compatible.)

The main visible differences are uncurrying (which may affect the public API of generated modules) and the details of what errors may be thrown.

Warning: bigint literals are currently unsupported and you will get JSON decoding errors if you use them in your source code.
You can encode them with arithmetic as a workaround until things are patched.

## Usage

`purs-backend-erl` compiles code from CoreFn in `./output` (`./output/Module.Name/corefn.json`) to Erlang modules in `./output-erl` (`./output-erl/Module.Name/module_name@{ps,foreign}.erl`).
This is the same convention as [`purerl`](https://github.com/purerl/purerl), just in `./output-erl` instead of `./output` to avoid conflicts.

Note: `purescript-backend-erl` is the name of the repository, while `purs-backend-erl` is the name of the executable and its associated [npm package](https://www.npmjs.com/package/purs-backend-erl).

### Tests

Run tests with `spago test -- --accept --run` to accept new golden output and compile and run the resulting Erlang tests.
(Not all tests have assertions (yet).)

## Results

Like `purescript-backend-optimizer`, performance improvement can be around 30% or even 40%.
The size of the compiled BEAM files is comparable to that of `purerl`.

## Optimizations

Optimizations are largely those provided by [`backend-optimizer`](https://github.com/aristanetworks/purescript-backend-optimizer), adapted as necessary for the Erlang code generation and purerl libraries.
(`Ref` and `ST` optimizations were yeeted.)

See the `backend-optimizer` repo for examples of how to inline RowList operations, Generics, lenses, and others.

The additional Erlang-specific optimizations take place at various stages of the pipeline:

- [Complexity analysis](https://github.com/id3as/purescript-backend-erl/blob/main/src/PureScript/Backend/Erl/Foreign/Analyze.purs) to guide inlining during `backend-optimizer`'s evaluation
- [Evaluation of known foreign functions in PS semantics](https://github.com/id3as/purescript-backend-erl/blob/main/src/PureScript/Backend/Erl/Foreign.purs) (they do not have to be foreign, strictly speaking)
- [Specializing known foreign functions during Erlang codegen](https://github.com/id3as/purescript-backend-erl/blob/main/src/PureScript/Backend/Erl/Convert/Foreign.purs)
  - This is where Erlang Lists and Tuples get specialized, since they work with Erlang data structures that cannot be represented by `backend-optimizer`.
- [Uncurrying during codegen](https://github.com/id3as/purescript-backend-erl/blob/main/src/PureScript/Backend/Erl/Convert.purs)
- [After Erlang codegen](https://github.com/id3as/purescript-backend-erl/blob/main/src/PureScript/Backend/Erl/Convert/After.purs)
  - This is Common Subexpression Elimination part 2, Erlang Edition.
  - Pattern matches are also generated based on demand analysis, to avoid deeply nested accessors.

The cherry on top is that Erlang has weird scoping rules, so the final step of codegen is [renaming local variables](https://github.com/id3as/purescript-backend-erl/blob/main/src/PureScript/Backend/Erl/Convert/Scoping.purs) throughout the whole AST.
Final beta-reduction of applied lambdas plus re-scoping of macros also happens there.

These optimizations were guided by inspecting the optimizations that Erlang performs, see [./opts](./opts/README.md).

### Uncurrying

Unlike `purerl`, which can access externs through the Haskell API of the compiler, `backend-erl` does not have access to the types of declarations.
This necessitates generating uncurrying based on the function *code* after optimization, not the *types*.
This makes it less predictable and may change the public API of PureScript modules in their Erlang form.

- The meaning of foreign imports remains the same: foreign `f/0` declarations are taken as-is, and `f/2` declarations are taken to be pure binary functions, and so on.
- Transitivity: If `g` calls `f` with two arguments and a `f/4` uncurrying exists, then a `g/2` uncurrying is also generated that calls `f/4`.
- Uncurrying is affected by `backend-optimizer` inlining directives (inlining `f arity=2` in the above example will also uncurry `g/2`, but via inlining this time, not by calling `f/4`).

The calling convention is that generated _nullary_ Erlang functions are always _pure_ wrappers around the code corresponding to a PureScript declaration.
Non-nullary *generated* Erlang functions may correspond to any of the three types of functions (curried, uncurried, or uncurried effectful), based upon their definition.
Non-nullary *FFI* functions only correspond to curried functions (`_ -> _ -> _`), not `FnX` nor `EffectFnX`.

### Inlining and specialization

- Foreign functions in core libraries are specialized and inlined when possible, especially Erlang BIFs.
- [Erlang Tuple](https://github.com/purerl/purescript-erl-tuples/blob/master/src/Erl/Data/Tuple.purs) operations are always inlined.
  [Erlang List](https://github.com/purerl/purescript-erl-lists/blob/master/src/Erl/Data/List.purs) operations are mostly inlined (TODO: inline `uncons`).
  They are both [treated as constructors](https://github.com/id3as/purescript-backend-erl/blob/main/src/PureScript/Backend/Erl/Foreign/Analyze.purs) by `backend-optimizer` so they will be inlined more aggressively.
- MagicDo is optimized only where the `backend-optimizer` inlinings make it obvious: abstract functions that are later called with `Effect` dictionaries do not have specializations generated (TODO).
- Foreign declarations are called directly, instead of via their wrappers in the generated PS modules (although those are still generated for backwards compatibility).

### Common Subexpression Elimination

Calls to top-level nullary functions are hoisted, to ensure that they are only instantiated once in a scope.

This is one of the main downsides of the Erlang codegen: top-level declarations have to be functions, so even with the CSE that `purs` itself performs, typeclass dictionaries get re-instantiated a lot.
(TODO: pull out typeclass methods as their own declarations to avoid dictionary creation in the first place.)

### Pattern matching

Some pattern matches are rewritten back to Erlang case trees for legibility (the `backend-optimizer` uses Boolean branching and unsafe accessors exclusively).
(TODO: do this for product-like datatypes.)

### TODO

- Pull out typeclass methods
- More uncurrying (e.g. for `compareImpl LT EQ GT`)
- Ensure generics always inline
- Test optimizations
- Make it extensible?
