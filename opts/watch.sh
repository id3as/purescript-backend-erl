#!/usr/bin/env bash
watchexec -w "$(dirname "$0")" -e sh,erl -r "$(dirname "$0")"/run.sh
