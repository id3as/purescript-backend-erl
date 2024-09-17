% Snapshot.LazyInit.Success
-module(snapshot_lazyInit_success@ps).
-export([ identity/0
        , identity/1
        , complicatedIdentity/0
        , complicatedIdentity/1
        , 'main.0'/0
        , main/0
        ]).
-compile(no_auto_import).
identity() ->
  fun identity/1.

identity(X) ->
  X.

complicatedIdentity() ->
  fun complicatedIdentity/1.

complicatedIdentity(_) ->
  begin
    H =
      fun
        (H, G, F) ->
          erlang:map_get(tick, (F(H, G, F))(10))
      end,
    G =
      fun
        (H@1, G, F) ->
          fun
            (N) ->
              erlang:map_get(tick, (F(H@1, G, F))(N))
          end
      end,
    F =
      fun
        (H@1, G@1, F) ->
          fun
            (N) ->
              #{ tick =>
                 if
                   N =< 0 ->
                     identity();
                   true ->
                     (erlang:map_get(tock, (F(H@1, G@1, F))(N - 1)))(identity())
                 end
               , tock =>
                 fun
                   (A) ->
                     ((G@1(H@1, G@1, F))(N))(A)
                 end
               }
          end
      end,
    H@1 = H(H, G, F),
    G(H, G, F),
    F(H, G, F),
    H@1
  end.

'main.0'() ->
  (complicatedIdentity(5))(unit).

main() ->
  fun
    () ->
      'main.0'()
  end.

