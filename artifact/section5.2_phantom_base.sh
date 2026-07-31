# set Zicond to false
sed -i '29s/true/false/' src/test/scala/test102_phantomCoh_range.scala
# run & trace
sbt "Test / runMain testVCDPhantomRange"
# reverse the change
sed -i '29s/false/true/' src/test/scala/test102_phantomCoh_range.scala
# Cleanup waveform file
rm /root/XS/build/*.vcd

