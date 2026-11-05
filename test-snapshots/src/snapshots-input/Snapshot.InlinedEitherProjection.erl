-module(snapshot_inlinedEitherProjection@foreign).
-export([lookup/1, check/1, crash/1]).

lookup(N) when N > 10 -> {left, <<"too big">>};
lookup(N) -> {right, N}.

check(N) when N > 5 -> {nothing};
check(N) -> {just, N}.

crash(S) -> error(S).
