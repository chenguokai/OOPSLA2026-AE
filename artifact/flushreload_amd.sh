sbt "Test / runMain TestFlushReload_IA"
mv /tmp/ga_run/Ga.elf /root/HT/flushreload-amd.elf
sed -i 's/val attack: Boolean = true/val attack: Boolean = false/' src/test/scala/test56_flush_reload.scala
sbt "Test / runMain TestFlushReload_IA"
sed -i 's/val attack: Boolean = false/val attack: Boolean = true/' src/test/scala/test56_flush_reload.scala
mv /tmp/ga_run/Ga.elf /root/HT/flushreload-base-amd.elf

