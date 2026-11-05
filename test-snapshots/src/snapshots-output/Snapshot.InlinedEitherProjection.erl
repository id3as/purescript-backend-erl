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
  <<"x", R#{ x => 1 }/binary>>.

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
        case case V@3 of
            {left, _} ->
              false;
            {right, _} ->
              true;
            _ ->
              erlang:error({fail, <<"Failed pattern match">>})
          end of
          true ->
            begin
              V@8 =
                case V@3 of
                  {left, #{ context := V@4 }} ->
                    V@4;
                  {right, #{ track := V@5 }} ->
                    V@5 + 1;
                  _ ->
                    erlang:error({fail, <<"Failed pattern match">>})
                end
                  + case V@3 of
                    {left, #{ renumbering := V@6 }} ->
                      V@6;
                    {right, #{ renumbering := V@7 }} ->
                      V@7;
                    _ ->
                      erlang:error({fail, <<"Failed pattern match">>})
                  end,
              fun
                () ->
                  V@8
              end
            end;
          _ ->
            case case V@3 of
                {left, _} ->
                  true;
                {right, _} ->
                  false;
                _ ->
                  erlang:error({fail, <<"Failed pattern match">>})
              end of
              true ->
                snapshot_inlinedEitherProjection@foreign:crash(case V@3 of
                  {left, V@9} ->
                    <<"cannot: ", V@9/binary, ".">>;
                  {right, #{ renumbering := V@10, track := V@11 }} ->
                    <<
                      "cannot: ",
                      #{ context => V@11 + 1, renumbering => V@10 }/binary,
                      "."
                    >>;
                  _ ->
                    erlang:error({fail, <<"Failed pattern match">>})
                end);
              _ ->
                erlang:error({fail, <<"Failed pattern match">>})
            end
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

