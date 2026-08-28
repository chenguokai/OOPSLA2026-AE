sbt "Test / runMain TestPhantom_AMD"
mv /tmp/ga_run/Ga.elf /root/HT/phantom-amd.elf
sed -i 's/val attack: Boolean = true/val attack: Boolean = false/' src/test/scala/test51_phantom-notfixed.scala
sbt "Test / runMain TestPhantom_AMD"
sed -i 's/val attack: Boolean = false/val attack: Boolean = true/' src/test/scala/test51_phantom-notfixed.scala
mv /tmp/ga_run/Ga.elf /root/HT/phantom-base-amd.elf

