-module(test).
-compile(export_all).

match(V) ->
  case V of
    {inl, _} -> 0;
    {inr, {inl, _}} -> 1;
    {inr, {inr, {inl, _}}} -> 2;
    {inr, {inr, {inr, _}}} -> 3
  end.
