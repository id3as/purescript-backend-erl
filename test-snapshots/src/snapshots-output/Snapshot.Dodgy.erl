-module(snapshot_dodgy@ps).
-export(['PrivateProcessTTimeoutMsg__'/0, 'ThereToGetRidOfUnreachableWarning'/0, noInline/0, noInline/1, isSecretMsg/0, isSecretMsg/1, result/0]).
-compile(no_auto_import).
'PrivateProcessTTimeoutMsg__'() ->
  {privateProcessTTimeoutMsg__}.
'ThereToGetRidOfUnreachableWarning'() ->
  {thereToGetRidOfUnreachableWarning}.
noInline() ->
  (fun
    (A@Local) ->
      (noInline(A@Local))
  end).
noInline(A) ->
  A.
isSecretMsg() ->
  (fun
    (V@Local) ->
      (isSecretMsg(V@Local))
  end).
isSecretMsg(V) ->
  ((erlang:is_tuple(V)) andalso ((1 =< (erlang:tuple_size(V))) andalso ((privateProcessTTimeoutMsg__ =:= (erlang:element(1, V))) andalso true))).
result() ->
  ((erlang:is_tuple(((snapshot_dodgy@ps:noInline())(1)))) andalso ((1 =< (erlang:tuple_size(((snapshot_dodgy@ps:noInline())(1))))) andalso ((privateProcessTTimeoutMsg__ =:= (erlang:element(1, ((snapshot_dodgy@ps:noInline())(1))))) andalso true))).
