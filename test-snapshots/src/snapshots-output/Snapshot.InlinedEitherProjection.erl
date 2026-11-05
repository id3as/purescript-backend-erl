% Snapshot.InlinedEitherProjection
-module(snapshot_inlinedEitherProjection@ps).
-export([ segment/0
        , segment/1
        , describe/0
        , describe/1
        , 'main.0'/0
        , main/0
        , lookup/0
        , check/0
        , crash/0
        ]).
-compile(no_auto_import).
segment() ->
  fun segment/1.

segment(R) ->
  <<"x", (R#{ x => 1 })/binary>>.

describe() ->
  fun describe/1.

describe(N) ->
  if
    N > 0 ->
      begin
        V = snapshot_inlinedEitherProjection@foreign:lookup(N),
        V@3 =
          case V of
            {left, V@1} ->
              fun
                (_) ->
                  {left, V@1}
              end;
            {right, V@2} ->
              fun
                (F) ->
                  F(V@2)
              end;
            _ ->
              erlang:error({fail, <<"Failed pattern match">>})
          end
          (fun
            (X) ->
              begin
                V@3 = snapshot_inlinedEitherProjection@foreign:check(X),
                case V@3 of
                  {nothing} ->
                    {left, <<"clash">>};
                  {just, V@4} ->
                    {right, #{ track => X, renumbering => V@4 }};
                  _ ->
                    erlang:error({fail, <<"Failed pattern match">>})
                end
              end
          end),
        case V@3 of
          {left, V@4} ->
            snapshot_inlinedEitherProjection@foreign:crash(<<
              "cannot: ",
              V@4/binary,
              "."
            >>);
          {right, #{ renumbering := V@5, track := V@6 }} ->
            begin
              V@7 = (V@6 + 1) + V@5,
              fun
                () ->
                  V@7
              end
            end;
          _ ->
            erlang:error({fail, <<"Failed pattern match">>})
        end
      end;
    true ->
      fun
        () ->
          0
      end
  end.

'main.0'() ->
  describe(1).

main() ->
  fun
    () ->
      begin
        V = data_show@ps:showInt(),
        V@1 = data_eq@ps:eqInt(),
        Three = ('main.0'())(),
        (test_assert@ps:'assertEqual\''(
           V@1,
           V,
           <<"">>,
           #{ expected => 3, actual => Three }
         ))(),
        Zero = (describe(0))(),
        (test_assert@ps:'assertEqual\''(
           V@1,
           V,
           <<"">>,
           #{ expected => 0, actual => Zero }
         ))()
      end
  end.

lookup() ->
  fun
    (V) ->
      snapshot_inlinedEitherProjection@foreign:lookup(V)
  end.

check() ->
  fun
    (V) ->
      snapshot_inlinedEitherProjection@foreign:check(V)
  end.

crash() ->
  fun
    (V) ->
      snapshot_inlinedEitherProjection@foreign:crash(V)
  end.

