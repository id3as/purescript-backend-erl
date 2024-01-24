#!/bin/bash
CWD=$PWD
cd $(dirname $0)
spago run -- --cwd "$CWD" "$@"
