# Modify the Lua test file
sed -i 's/cc \& 1/cc % 2/g' /root/ELF/riscv-rootfs/test.lua
# run & trace
sbt "Test / runMain TestLuaInterpreter_XS"
# Revert the change
sed -i 's/cc % 2/cc \& 1/g' /root/ELF/riscv-rootfs/test.lua
# Cleanup waveform file
rm /root/XS/build/*.vcd

