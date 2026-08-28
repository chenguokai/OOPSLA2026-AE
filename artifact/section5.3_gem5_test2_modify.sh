#!/usr/bin/env bash

FILE="/root/gem5/test2.lua"
TMP="${FILE}.tmp"

cp "$FILE" "${FILE}.bak" || exit 1

awk '
{
    # Start of a commented-out target block
    if ($0 ~ /^[[:space:]]*--[[:space:]]*if[[:space:]]+f[[:space:]]*==[[:space:]]*0[[:space:]]+then[[:space:]]*$/) {
        in_block = 1
    }

    if (in_block) {
        # Remove the Lua comment marker only from this block
        sub(/--[[:space:]]?/, "")
        print

        # End of the commented-out if block
        if ($0 ~ /^[[:space:]]*end[[:space:]]*$/) {
            in_block = 0
        }

        next
    }

    print
}
' "$FILE" > "$TMP" && mv "$TMP" "$FILE"
