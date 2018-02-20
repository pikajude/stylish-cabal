#!/bin/bash

nix-build "$(dirname "$0")/build-all.nix" --no-out-link "$@"
