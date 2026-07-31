# set Zicond to false
sed -i '20s/val zicond: Boolean = true/val zicond: Boolean = false/' src/test/scala/test105_zicond.scala
# noRandom
sed -i '22s/false/true/' src/test/scala/test105_zicond.scala
# run & trace
sbt "Test / runMain TestZicond_XS"
# reverse the change
sed -i '20s/val zicond: Boolean = false/val zicond: Boolean = true/' src/test/scala/test105_zicond.scala
sed -i '22s/true/false/' src/test/scala/test105_zicond.scala
# Cleanup waveform file
rm /root/XS/build/*.vcd

