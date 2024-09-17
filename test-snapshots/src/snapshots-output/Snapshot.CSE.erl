% Snapshot.CSE
-module(snapshot_cSE@ps).
-export(['D'/0, ord/0, ord/2]).
-compile(no_auto_import).
'D'() ->
  fun
    (Value0) ->
      fun
        (Value1) ->
          {d, Value0, Value1}
      end
  end.

ord() ->
  fun
    (V) ->
      fun
        (V1) ->
          ord(V, V1)
      end
  end.

ord(V, V1) ->
  begin
    V@1 = data_ord@ps:ordInt(),
    V@2 =
      ((erlang:map_get(compare, V@1))(erlang:element(2, V)))
      (erlang:element(2, V1)),
    V@3 =
      ((erlang:map_get(compare, V@1))(erlang:element(3, V)))
      (erlang:element(3, V1)),
    case V@2 of
      {lT} ->
        {lT};
      {gT} ->
        {gT};
      {eQ} ->
        V@3;
      _ ->
        erlang:error({fail, <<"Failed pattern match">>})
    end
  end.

