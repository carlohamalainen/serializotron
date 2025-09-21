#!/bin/bash

set -e
set -x

export PATH=$HOME/.local/bin:$PATH

protoc --plugin=protoc-gen-haskell=`which proto-lens-protoc` \
    --proto_path=proto \
    --haskell_out=src \
    proto/serializotron.proto