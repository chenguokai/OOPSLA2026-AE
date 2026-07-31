# set Zicond to false
sed -i '51s/false/true/' src/test/scala/test111_luaEval.scala
# run & trace
sbt "Test / runMain TestLuaInterpreter_XS"
# reverse the change
sed -i '51s/true/false/' src/test/scala/test111_luaEval.scala
# Cleanup waveform file
rm /root/XS/build/*.vcd

