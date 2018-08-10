#!/usr/bin/env bash

tmux resize-pane -x 80

$(nix-build ../shell.nix)/bin/elm make src/Main.elm --output elm.js

while inotifywait -e modify -q -r src ../src; do
    $(nix-build ../shell.nix)/bin/elm make src/Main.elm --output elm.js
done
