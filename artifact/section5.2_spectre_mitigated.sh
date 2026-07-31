# set mitigated to true
sed -i '25s/false/true/' src/test/scala/test104_spectre_range.scala
# run & trace
sbt "Test / runMain TestSpectreRange_XS"
# reverse the change
sed -i '25s/true/false/' src/test/scala/test104_spectre_range.scala
# Cleanup waveform file
rm /root/XS/build/*.vcd

