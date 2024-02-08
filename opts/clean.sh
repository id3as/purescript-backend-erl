#!/usr/bin/env bash
cd -- "$(dirname $0)"
for d in *; do
  if [ -d "$d" ]; then
    for f in "$d"/*.erl; do
      b="$(basename $f .erl)"
      rm -rf "$d/opt"
      rm -rf "$d/no_opt"
      rm -rf "$d/$b".*.diff
    done
  fi
done
