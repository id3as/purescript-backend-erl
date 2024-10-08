#!/usr/bin/env bash
set -euo pipefail
shopt -s nullglob
diff="git diff --no-index -U1000"
export ERLC_USE_SERVER=true
export ERLC_SERVER_ID=opts_test
# Each directory is a suite of tests to try
for d in "$(dirname "$0")"/*; do
  if [ -d "$d" ]; then
    # For each file we run some optimizations and compute their diffs
    for f in "$d"/*.erl; do
      if [ ! -s $f ]; then
        cat "$(dirname "$0")"/template.erl >> $f
      fi
      b="$(basename "$f" .erl)"
      mkdir -p "$d/opt"
      (set -x; erlc -o "$d/opt" -W0 -S "$f")
      (set -x; erlc -o "$d/opt" -W0 +dssa "$f")
      (set -x; erlc -o "$d/opt" -W0 +dcopt "$f")
      mv "$d/opt/$b.copt" "$d/opt/$b.core"
      mkdir -p "$d/unopt"
      (set -x; erlc -o "$d/unopt" -W0 -S +no_ssa_opt "$f")
      (set -x; erlc -o "$d/unopt" -W0 +dssa +no_ssa_opt "$f")
      (set -x; erlc -o "$d/unopt" -W0 +dcore "$f")
      $diff "$d/unopt/$b.S" "$d/opt/$b.S" > "$d/$b.S.diff" || true
      $diff "$d/unopt/$b.ssa" "$d/opt/$b.ssa" > "$d/$b.ssa.diff" || true
      $diff "$d/unopt/$b.core" "$d/opt/$b.core" > "$d/$b.core.diff" || true
    done
    # Finally we compare no_$b.erl to yes_$b.erl
    for f in "$d"/no_*.erl; do
      b="$(basename "$f" .erl | cut -c 4-)"
      if [ -f "$d/yes_$b.erl" ] && [ -f "$d/no_$b.erl" ]; then
        $diff "$d/yes_$b.erl" "$d/no_$b.erl" >/dev/null && echo "Warning: $d/yes_$b.erl and $d/no_$b.erl are the same" || true
        for o in unopt opt; do
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
