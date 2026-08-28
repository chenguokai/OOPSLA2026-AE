#!/bin/sh
set -eu

file="src/test/scala/test105_zicond.scala"

if grep -q 'val t = UInt64(2026)' "$file"; then
    echo "Already patched: $file"
    exit 0
fi

sed -i '/val main = Func(SInt)() {/a\
      val t = UInt64(2026)\
      printInt(t)
' "$file"

echo "Updated: $file"
