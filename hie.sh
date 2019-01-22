#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")" || exit 1

argv=( "$@" )
argv=( "${argv[@]/\'/\'\\\'\'}" )
argv=( "${argv[@]/#/\'}" )
argv=( "${argv[@]/%/\'}" )

exec nix-shell --pure --run "hie-wrapper ${argv[*]}"
