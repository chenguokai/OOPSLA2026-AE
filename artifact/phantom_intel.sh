sbt "Test / runMain testVCDPhantomRange_IA"
mv /tmp/ga_run/Ga.elf /root/HT/phantom-intel.elf
sed -i 's/val attack: Boolean = true/val attack: Boolean = false/' src/test/scala/test102_phantomCoh_range.scala
sbt "Test / runMain testVCDPhantomRange_IA"
sed -i 's/val attack: Boolean = false/val attack: Boolean = true/' src/test/scala/test102_phantomCoh_range.scala
mv /tmp/ga_run/Ga.elf /root/HT/phantom-base-intel.elf

