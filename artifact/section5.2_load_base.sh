# set Zicond to false
sed -i '111s/true/false/' src/test/scala/test103_loadvio_range.scala
# run & trace
sbt "Test / runMain TestLoadvio_XSRange"
# reverse the change
sed -i '111s/false/true/' src/test/scala/test103_loadvio_range.scala
# Cleanup waveform file
rm /root/XS/build/*.vcd

