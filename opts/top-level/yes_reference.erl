-module(test).
-compile(export_all).

rule1() ->
  #{ category => 2
   , unicodeCat => {nUMCAT_ZS}
   , possible => 0
   , updist => 0
   , lowdist => 0
   , titledist => 0
   }.

spacechars() ->
  begin
    % not inlined, despite being a local call :/
    V = rule1(),
    array:from_list([ #{ start => 32, length => 1, convRule => V }
                    , #{ start => 160, length => 1, convRule => V }
                    , #{ start => 5760, length => 1, convRule => V }
                    , #{ start => 8192, length => 11, convRule => V }
                    , #{ start => 8239, length => 1, convRule => V }
                    , #{ start => 8287, length => 1, convRule => V }
                    , #{ start => 12288, length => 1, convRule => V }
                    ])
  end.
