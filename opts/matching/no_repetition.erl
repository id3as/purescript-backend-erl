-module(test).
-compile(export_all).

match(V) ->
  case V of
    {inl, _} -> 0;
    {inr, V1} ->
      case V1 of
        {inl, _} -> 1;
        {inr, V2} ->
          case V2 of
            {inl, _} -> 2;
            {inr, _} -> 3
          end
      end
  end.
