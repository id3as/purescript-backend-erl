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
-define( IS_TAG(Tag, V)
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
    (A@Local) ->
      noInline(A@Local)
  end.

noInline(A) ->
  A.

isSecretMsg() ->
  fun
    (V@Local) ->
      isSecretMsg(V@Local)
  end.

isSecretMsg(V) ->
  ?IS_TAG(privateProcessTTimeoutMsg__, V).

result() ->
  ?IS_TAG(privateProcessTTimeoutMsg__, (snapshot_dodgy@ps:noInline())(1)).

