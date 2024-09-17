% Snapshot.Let.GoArity
-module(snapshot_let_goArity@ps).
-export([foldr/0, foldr/2, result/0]).
-compile(no_auto_import).
foldr() ->
  fun
    (F) ->
      fun
        (B) ->
          foldr(F, B)
      end
  end.

foldr(F, B) ->
  begin
    Go =
      fun
        Go (B@1, V) ->
          case V of
            {nil} ->
              B@1;
            {cons, V@1, V@2} ->
              begin
                B@2 = (F(V@1))(B@1),
                Go(B@2, V@2)
              end;
            _ ->
              erlang:error({fail, <<"Failed pattern match">>})
          end
      end,
    V =
      fun
        (V) ->
          Go(B, V)
      end,
    Go@1 =
      fun
        Go@1 (V@1, V1) ->
          case V1 of
            {nil} ->
              V@1;
            {cons, V1@1, V1@2} ->
              begin
                V@2 = {cons, V1@1, V@1},
                Go@1(V@2, V1@2)
              end;
            _ ->
              erlang:error({fail, <<"Failed pattern match">>})
          end
      end,
    V@2 =
      begin
        V@1 = {nil},
        fun
          (V1) ->
            Go@1(V@1, V1)
        end
      end,
    fun
      (X) ->
        V(V@2(X))
    end
  end.

result() ->
  (foldr(data_semiring@ps:intAdd(), 1))({cons, 2, {cons, 3, {cons, 4, {nil}}}}).

