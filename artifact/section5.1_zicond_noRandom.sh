# noRandom
sed -i '22s/false/true/' src/test/scala/test105_zicond.scala
# run & trace
sbt "Test / runMain TestZicond_XS"
# noRandom restore
sed -i '22s/true/false/' src/test/scala/test105_zicond.scala
# Cleanup waveform file
rm /root/XS/build/*.vcd

