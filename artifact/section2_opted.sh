# set opt to true
sed -i '19s/false/true/' src/test/scala/test106_dcache_idx.scala
# run & trace
sbt "Test / runMain TestDCacheIdx_XS"
# reverse the change
sed -i '19s/true/false/' src/test/scala/test106_dcache_idx.scala
# Cleanup waveform file
rm /root/XS/build/*.vcd

