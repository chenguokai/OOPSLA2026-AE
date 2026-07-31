cd /root/gem5
# Adjust the Lua script
sed -i \
  -e '/^[[:space:]]*c={[[:space:]]*1,1,1,0,0,0,1,1,1,0/s/^\([[:space:]]*\)/\1-- /' \
  -e '/^[[:space:]]*--[[:space:]]*c = {[[:space:]]*1, 1, 1, 1, 1, 1, 0, 0, 0/s/^\([[:space:]]*\)--[[:space:]]*/\1/' \
  /root/gem5/test.lua
build/X86/gem5.opt ./se-cache.py
cat commit.txt reg.txt mispred.txt fetch.txt dcache.txt pred.txt  > gem5-trace.txt
rm dcache.txt fetch.txt commit.txt pred.txt reg.txt mispred.txt
cd /root/HT
sbt "Test / runMain TestLuaInterpreter_Gem5"
rm /root/gem5/gem5-trace.txt
# Revert the Lua changes
sed -i \
  -e '/^[[:space:]]*--[[:space:]]*c={[[:space:]]*1,1,1,0,0,0,1,1,1,0/s/^\([[:space:]]*\)--[[:space:]]*/\1/' \
  -e '/^[[:space:]]*c = {[[:space:]]*1, 1, 1, 1, 1, 1, 0, 0, 0/s/^\([[:space:]]*\)/\1-- /' \
  /root/gem5/test.lua
