#!/usr/bin/env bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
pushd $SCRIPT_DIR >/dev/null
echo "[$(date)] Building..."
elm make ./src/GuerillaCheckers.elm --output build/elm.js >/dev/null
if [[ $? -ne 0 ]]; then
    echo "[$(date)] Build failed; see output above for details"
    popd >/dev/null
    exit 1
fi

if [[ -d ./dist ]]; then
    rm -rf ./dist
fi
mkdir ./dist
cp index.html ./dist
cp -r build ./dist
cp style.css ./dist
echo "[$(date)] Build complete!"
popd >/dev/null
