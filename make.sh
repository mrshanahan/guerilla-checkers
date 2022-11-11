#!/usr/bin/env bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
pushd $SCRIPT_DIR
elm make ./src/GuerillaCheckers.elm --output build/elm.js
if [[ -d ./dist ]]; then
    rm -rf ./dist
fi
mkdir ./dist
cp index.html ./dist
cp -r build ./dist
cp style.css ./dist
