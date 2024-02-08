-module(test).
-compile(export_all).

fromFoldable() ->
  begin
    Go =
      fun
        Go (B, V) ->
          case V of
            {nil} ->
              B;
            {cons, V@1, V@2} ->
              ((fun
                 (B@1) ->
                   fun
                     (V@3) ->
                       Go(B@1, V@3)
                   end
               end)
               (erl_data_map@foreign:insert(
                  erlang:element(2, V@1),
                  erlang:element(3, V@1),
                  B
                )))
              (V@2);
            _ ->
              erlang:error({fail, <<"Failed pattern match">>})
          end
      end,
    (fun
      (B) ->
        fun
          (V) ->
            Go(B, V)
        end
    end)
    (#{})
  end.
