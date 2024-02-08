#!/usr/bin/env bash
set -euo pipefail
diff="git diff --no-index -U1000"
cd -- "$(dirname $0)"
# Each directory is a suite of tests to try
for d in ./*; do
  if [ -d "$d" ]; then
    # For each file we run some optimizations and compute their diffs
    for f in "$d"/*.erl; do
      b="$(basename "$f" .erl)"
      mkdir -p "$d/opt"
      erlc -o "$d/opt" -W0 -S "$f"
      erlc -o "$d/opt" -W0 +dssa "$f"
      erlc -o "$d/opt" -W0 +dcopt "$f"
      mv "$d/opt/$b.copt" "$d/opt/$b.core"
      mkdir -p "$d/no_opt"
      erlc -o "$d/no_opt" -W0 -S +no_ssa_opt "$f"
      erlc -o "$d/no_opt" -W0 +dssa +no_ssa_opt "$f"
      erlc -o "$d/no_opt" -W0 +dcore "$f"
      $diff "$d/no_opt/$b.S" "$d/opt/$b.S" > "$d/$b.S.diff" || true
      $diff "$d/no_opt/$b.ssa" "$d/opt/$b.ssa" > "$d/$b.ssa.diff" || true
      $diff "$d/no_opt/$b.core" "$d/opt/$b.core" > "$d/$b.core.diff" || true
    done
    # Finally we compare no_$b.erl to yes_$b.erl
    for f in "$d"/no_*.erl; do
      # Guard against empty glob failure, mainly
      if [ -f "$f" ]; then
        b="$(basename "$f" .erl | cut -c 4-)"
        $diff "$d/yes_$b.erl" "$d/no_$b.erl" >/dev/null && echo "Warning: $d/yes_$b.erl and $d/no_$b.erl are the same" || true
        for o in no_opt opt; do
          for ext in ssa core S; do
            # Requires git 2.42: https://stackoverflow.com/questions/22706714/why-does-git-diff-not-work-with-process-substitution#answer-76714718
            $diff <(sed "s/yes_$b/$b/g" "$d/$o/yes_$b.$ext") <(sed "s/no_$b/$b/g" "$d/$o/no_$b.$ext") |
              sed "s!a/dev/fd/\\w\\+!$d/$o/yes_$b.$ext!g;s!b/dev/fd/\\w\\+!$d/$o/no_$b.$ext!g" > "$d/$o/$b.$ext.diff" \
                || true
          done
        done
      fi
    done
  fi
done
