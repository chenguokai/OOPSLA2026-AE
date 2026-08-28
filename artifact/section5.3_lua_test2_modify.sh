#!/usr/bin/env bash

FILE="/root/ELF/riscv-rootfs/test2.lua"
TMP="${FILE}.tmp"

cp "$FILE" "${FILE}.bak" || exit 1

awk '
{
    if ($0 ~ /^[[:space:]]*--[[:space:]]*if[[:space:]]+f[[:space:]]*==[[:space:]]*0[[:space:]]+then[[:space:]]*$/) {
        in_block = 1
    }

    if (in_block) {
        sub(/--[[:space:]]?/, "")
        print

        if ($0 ~ /^[[:space:]]*end[[:space:]]*$/) {
            in_block = 0
        }

        next
    }

    print
}
' "$FILE" > "$TMP" && mv "$TMP" "$FILE"
