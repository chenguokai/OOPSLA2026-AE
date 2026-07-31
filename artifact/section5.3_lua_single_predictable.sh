sed -i '
/^-- c={[[:space:]]*1,1,1,0,0,0/ s/^--[[:space:]]*//
/^c = {[[:space:]]*1, 1, 1, 1, 1, 1, 0/ s/^/-- /
' /root/ELF/riscv-rootfs/test.lua

# run & trace
sbt "Test / runMain TestLuaInterpreter_XS"
# Revert the change
sed -i '
/^c={[[:space:]]*1,1,1,0,0,0/ s/^/-- /
/^-- c = {[[:space:]]*1, 1, 1, 1, 1, 1, 0/ s/^--[[:space:]]*//
' /root/ELF/riscv-rootfs/test.lua
# Cleanup waveform file
rm /root/XS/build/*.vcd

