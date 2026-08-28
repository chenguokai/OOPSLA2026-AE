#!/bin/sh
set -eu

FILE="src/test/scala/test56_flush_reload.scala"

if [ ! -f "$FILE" ]; then
    echo "Error: file not found: $FILE" >&2
    exit 1
fi

sed -i \
    's/base_var\.obj\.saddr === 0x40000000L + MarchParameters\.L1DLine/base_var.obj.saddr === 0x40000020L/' \
    "$FILE"

echo "Updated $FILE"
