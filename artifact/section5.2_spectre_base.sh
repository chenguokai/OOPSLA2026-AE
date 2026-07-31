# set Zicond to false
sed -i '24s/true/false/' src/test/scala/test104_spectre_range.scala
# run & trace
sbt "Test / runMain TestSpectreRange_XS"
# reverse the change
sed -i '24s/false/true/' src/test/scala/test104_spectre_range.scala
# Cleanup waveform file
rm /root/XS/build/*.vcd

