-module(snapshot_dodgy@ps).
-export([ 'PrivateProcessTTimeoutMsg__'/0
        , 'ThereToGetRidOfUnreachableWarning'/0
        , noInline/0
        , noInline/1
        , isSecretMsg/0
        , isSecretMsg/1
        , result/0
        ]).
-compile(no_auto_import).
-define( IS_KNOWN_TAG(Tag, Arity, V)
       , ((erlang:is_tuple(V))
         andalso ((1 =< (erlang:tuple_size(V)))
           andalso (Tag =:= (erlang:element(1, V)))))
       ).
'PrivateProcessTTimeoutMsg__'() ->
  {privateProcessTTimeoutMsg__}.

'ThereToGetRidOfUnreachableWarning'() ->
  {thereToGetRidOfUnreachableWarning}.

noInline() ->
  fun
    (A) ->
      noInline(A)
  end.

noInline(A) ->
  A.

isSecretMsg() ->
  fun
    (V) ->
      isSecretMsg(V)
  end.

isSecretMsg(V) ->
  ?IS_KNOWN_TAG(privateProcessTTimeoutMsg__, 0, V).

result() ->
  begin
    V = noInline(1),
    ?IS_KNOWN_TAG(privateProcessTTimeoutMsg__, 0, V)
  end.

