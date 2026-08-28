#!/bin/sh
set -eu

FILE="src/test/scala/test106_dcache_idx.scala"

if [ ! -f "$FILE" ]; then
    echo "Error: file not found: $FILE" >&2
    exit 1
fi

sed -i 's/emu_path = "emu-dcache-base"/emu_path = "emu"/' "$FILE"

echo "Updated $FILE"
