#!/bin/sh

while inotifywait -e modify --exclude ".*\.so" --exclude ".*_flymake.hs" --exclude "\.#.*" -r . ; do cabal build ; done

