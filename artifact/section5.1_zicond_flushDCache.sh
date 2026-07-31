# flushDCache
sed -i '23s/false/true/' src/test/scala/test105_zicond.scala
# run & trace
sbt "Test / runMain TestZicond_XS"
# flushDCache restore
sed -i '23s/true/false/' src/test/scala/test105_zicond.scala
# Cleanup waveform file
rm /root/XS/build/*.vcd

