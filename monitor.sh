#!/usr/bin/env bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
pushd $SCRIPT_DIR >/dev/null
fswatch --exclude ".*" --include "$(pwd)/[^/]+\\.(elm|html|css)$" --include "$(pwd)/src/[^/]+\\.(elm|html|css)$" --event Updated --event Removed --event Created ./ -xrE | go run ./batch.go -i 3 | xargs -0 -L 1 ./build.sh
popd >/dev/null
