#!/bin/sh
set -eu

PROJECT_ROOT=$(pwd -P)
TARGET_FILE="$PROJECT_ROOT/src/test/scala/test105_zicond_demo.scala"
VCD_PATH="$PROJECT_ROOT/test.vcd"

if [ ! -f "$TARGET_FILE" ]; then
    echo "Error: target file not found: $TARGET_FILE" >&2
    exit 1
fi

PLACEHOLDER='val vcd_path = "/PATH/TO/test.vcd"'

if ! grep -Fq "$PLACEHOLDER" "$TARGET_FILE"; then
    echo "Error: VCD path placeholder not found in $TARGET_FILE" >&2
    exit 1
fi

# Escape characters that have special meaning in sed replacement strings.
ESCAPED_VCD_PATH=$(printf '%s\n' "$VCD_PATH" | sed 's/[\\&|]/\\&/g')

TMP_FILE="$TARGET_FILE.tmp.$$"
trap 'rm -f "$TMP_FILE"' EXIT HUP INT TERM

sed \
    "s|val vcd_path = \"/PATH/TO/test.vcd\"|val vcd_path = \"$ESCAPED_VCD_PATH\"|" \
    "$TARGET_FILE" > "$TMP_FILE"

cat "$TMP_FILE" > "$TARGET_FILE"
rm -f "$TMP_FILE"
trap - EXIT HUP INT TERM

echo "Patched VCD path to: $VCD_PATH"
