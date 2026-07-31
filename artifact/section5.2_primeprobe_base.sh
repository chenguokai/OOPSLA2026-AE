# set attack to false
sed -i '23s/true/false/' src/test/scala/test21_prime_probe_dsl_flavor.scala
# run & trace
sbt "Test / runMain TestPrimeProbeDSL_XS"
# reverse the change
sed -i '23s/false/true/' src/test/scala/test21_prime_probe_dsl_flavor.scala
# Cleanup waveform file
rm /root/XS/build/*.vcd

