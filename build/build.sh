#!/usr/bin/env bash

cd "$(dirname "$0")"
stack build
stack exec --verbosity=error -- ./collect_objects.pl $(stack exec --verbosity=error -- ghc-pkg list --show-unit-ids --simple-output llvm-rewriting-rules)
clang++ ../pluginMain.cpp --std=c++17 --shared -o rulesPlugin.so -Llib $(cat incopts.txt) $(cat ldopts.txt) -Wl,-rpath,$(pwd)/lib
