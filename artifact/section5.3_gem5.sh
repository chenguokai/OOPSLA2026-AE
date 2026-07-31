cd /root/gem5
build/X86/gem5.opt ./se-cache.py
cat commit.txt reg.txt mispred.txt fetch.txt dcache.txt pred.txt  > gem5-trace.txt
rm dcache.txt fetch.txt commit.txt pred.txt reg.txt mispred.txt
cd /root/HT
sbt "Test / runMain TestLuaInterpreter_Gem5"
rm /root/gem5/gem5-trace.txt
