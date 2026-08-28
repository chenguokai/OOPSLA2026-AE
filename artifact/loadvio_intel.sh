sbt "Test / runMain TestLoadvio_IARange"
mv /tmp/ga_run/Ga.elf /root/HT/loadvio-intel.elf
sed -i 's/val attack:Boolean = true/val attack:Boolean = false/' src/test/scala/test103_loadvio_range.scala
sbt "Test / runMain TestLoadvio_IARange"
sed -i 's/val attack:Boolean = false/val attack:Boolean = true/' src/test/scala/test103_loadvio_range.scala
mv /tmp/ga_run/Ga.elf /root/HT/loadvio-base-intel.elf

