sbt "Test / runMain TestSpectreRange_IA"
mv /tmp/ga_run/Ga.elf /root/HT/spectre-intel.elf
sed -i 's/val attack: Boolean = true/val attack: Boolean = false/' src/test/scala/test104_spectre_range.scala
sbt "Test / runMain TestSpectreRange_IA"
sed -i 's/val attack: Boolean = false/val attack: Boolean = true/' src/test/scala/test104_spectre_range.scala
mv /tmp/ga_run/Ga.elf /root/HT/spectre-base-intel.elf

