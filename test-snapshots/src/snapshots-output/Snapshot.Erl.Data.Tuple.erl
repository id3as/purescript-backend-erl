-module(snapshot_erl_data_tuple@ps).
-export([ uncurriedMore/0
        , uncurried/0
        , tf2/0
        , t5/0
        , t2/0
        , sndt2/0
        , r5/0
        , result/0
        , fsttf2/0
        , fstt2/0
        , uncurriedMore/1
        , uncurried/1
        ]).
-compile(no_auto_import).
uncurriedMore() ->
  fun
    ({A, B, C, D, E}) ->
      fun
        (F) ->
          fun
            ({G, H}) ->
              {(A + B) + G, <<C/binary, D/binary, (F(E))/binary, H/binary>>}
          end
      end
  end.

uncurried() ->
  fun
    ({A, B, C, D, _}) ->
      {A + B, <<C/binary, D/binary>>}
  end.

tf2() ->
  { fun
      (I) ->
        I - 5
    end
  , <<"7">>
  }.

t5() ->
  {3, 4, <<"hi">>, <<"there">>, $V}.

t2() ->
  {4, <<"hi">>}.

sndt2() ->
  <<"hi">>.

r5() ->
  {7, <<"hithere">>}.

result() ->
  ((uncurriedMore({3, 4, <<"hi">>, <<"there">>, $V}))
   (fun
     (_) ->
       <<"">>
   end))
  ({7, <<"hithere">>}).

fsttf2() ->
  (erl_data_tuple@foreign:fst(tf2()))(12).

fstt2() ->
  4.

uncurriedMore(V) ->
  begin
    {A, B, C, D, E} = V,
    fun
      (F) ->
        fun
          ({G, H}) ->
            {(A + B) + G, <<C/binary, D/binary, (F(E))/binary, H/binary>>}
        end
    end
  end.

uncurried(V) ->
  begin
    {A, B, C, D, _} = V,
    {A + B, <<C/binary, D/binary>>}
  end.

