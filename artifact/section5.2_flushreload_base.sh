sed -i '20s/true/false/' src/test/scala/test56_flush_reload.scala
# run & trace
sbt "Test / runMain TestFlushReload_XS"
sed -i '20s/false/true/' src/test/scala/test56_flush_reload.scala
# Cleanup waveform file
rm /root/XS/build/*.vcd

