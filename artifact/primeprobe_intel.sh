sbt "Test / runMain TestPrimeProbeDSL_IA"
mv /tmp/ga_run/Ga.elf /root/HT/primeprobe-intel.elf
sed -i 's/val attack: Boolean = true/val attack: Boolean = false/' src/test/scala/test21_prime_probe_dsl_flavor.scala
sbt "Test / runMain TestPrimeProbeDSL_IA"
sed -i 's/val attack: Boolean = false/val attack: Boolean = true/' src/test/scala/test21_prime_probe_dsl_flavor.scala
mv /tmp/ga_run/Ga.elf /root/HT/primeprobe-base-intel.elf

