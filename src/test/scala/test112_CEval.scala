import HT.ASTNodes.*
import HT.ASTUtils.printAST
import HT.CachelineUtils.EvictSetFromL1DCacheline
import HT.CodeGen.*
import HT.Executors.pyStr2Result
import HT.Permissions.Permission.VictimPublic
import HT.StdLib.*
import HT.Types.*
import HT.*
import HT.RISCV64.rvRegToInt
import HT.observation.*
import HT.observation.AbstractionLayer.*
import HT.simrunners.XSRun

import scala.collection.mutable.ArrayBuffer

def test112body = {
  val ELFPath = "/home/xim-intel14/ELF/riscv-rootfs/lua-equ-hard.elf"
  //  "/mnt/ssd4t/home/xim-intel14/luahard.vcd" "/mnt/ssd4t/home/xim-intel14/lua-correlation.vcd"
  val vcd_path = XSRun(1000000, customELF = Some(ELFPath))
  val luaPC = 0x10740
  println(f"Inst that load Lua PC: 0x${luaPC}%08x")
  val parser = VCDParser(vcd_path.toString, XSBPUSignalSet ++ XSROBSignalSet)

  val bpuObj = new XSBPU(parser)

  setGraphRange(0, parser.getMaxTime.toInt)

  GlobalCycle = 0 // reset cycle for next analysis

  var mispredMap = Map[BigInt, Int]()

  val mispredCond = () => {
    bpuObj.hasMispred(GlobalCycle)
  }

  On(mispredCond, "mispredictions") {
    entry{
      val pc = bpuObj.MispredPC(GlobalCycle)
      mispredMap += (pc -> (mispredMap.getOrElse(pc, 0) + 1))
      EventLog(f"Mispred PC: 0x${pc.toLong}%X at cycle $GlobalCycle")
    }
  }
  println("Misprediction Map:")
  mispredMap.toList.sortBy(-_._2).foreach { case (pc, count) =>
    println(f"PC: 0x${pc.toLong}%016x, Count: $count, ELF Pos: ${getELFPos(pc.toLong)}")
  }
  generateGraph()
}

@main def TestLuaEquivalence_XS = {
  applyXiangShan2ndGenParam()
  test112body
}