#!/bin/bash
CWD=$PWD
cd $(dirname $0)
spago run --main PureScript.Backend.Erl.Main -- --cwd "$CWD" "$@"
