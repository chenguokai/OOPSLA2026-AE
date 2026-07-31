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

def lookupClosestLessThan(cycle: Int, commitMap: ArrayBuffer[(Int, Long)]): Long = {
  if (commitMap.isEmpty) {
    throw new NoSuchElementException("Commit map is empty")
  }

  // Check if the cycle is smaller than the first element
  if (cycle < commitMap(0)._1) {
    return 0
  }

  // Check if the cycle is greater than or equal to the last element
  if (cycle >= commitMap.last._1) {
    return commitMap.last._2
  }

  // Binary search
  var low = 0
  var high = commitMap.length - 1

  while (low < high) {
    val mid = low + (high - low + 1) / 2  // Using ceiling to ensure progress

    if (commitMap(mid)._1 <= cycle) {
      low = mid
    } else {
      high = mid - 1
    }
  }

  // At this point, low == high and commitMap(low)._1 <= cycle
  commitMap(low)._2
}

def test111body = {
  val test2: Boolean = false
  val ELFPath = if (test2)"/root/ELF/riscv-rootfs/luainit2.elf" else "/root/ELF/riscv-rootfs/luainit.elf"
  val luaPath = "/root/lua/lua-rv"
  ELFParseInit(luaPath)
  println("Debug: fileNumbers: " + fileNumbers)
  val luaPC = getELFAddr("lvm.c:1653").head
  println(f"Inst that load Lua PC: 0x${luaPC}%08x")
  val vcd_path = XSRun(1000000, customELF = Some(ELFPath)) // "/home/xim-intel14/XS///////build/2025-10-07@16:00:31.vcd" //
  val parser = VCDParser(vcd_path.toString, XSBPUSignalSet ++ XSROBSignalSet)

  val bpuObj = new XSBPU(parser)
  val robObj = new XSROB(parser)

  setGraphRange(0, parser.getMaxTime.toInt)

  var commitMap = ArrayBuffer[(Int, Long)]() // array of pairs (cycle, pc)

  val commitCond = () => {
    robObj.commitWithPC(GlobalCycle, luaPC)
  }

  On(commitCond, "Lua PC inst commit") {
    entry{
      val opcodePC = robObj.getIntReg(GlobalCycle, "t0")
      commitMap += ((GlobalCycle, opcodePC))
    }
  }

  println("Commit Map (Cycle, Lua PC):")
  commitMap.foreach { case (cycle, pc) =>
    println(f"Cycle: $cycle, Lua PC: ${pc}")
  }

  GlobalCycle = 0 // reset cycle for next analysis

  var mispredMap = Map[BigInt, Int]()

  val mispredCond = () => {
    bpuObj.hasMispred(GlobalCycle)
  }

  On(mispredCond, "mispredictions") {
    entry{
      val pc = bpuObj.MispredPC(GlobalCycle)
      mispredMap +=
        (pc -> (mispredMap.getOrElse(pc, 0) + 1))
      EventLog(f"Mispred PC: 0x${pc.toLong}%X at cycle $GlobalCycle Lua PC: 0x${lookupClosestLessThan(GlobalCycle, commitMap)}%08x")
    }
  }
  println("Misprediction Map:")
  mispredMap.toList.sortBy(-_._2).foreach {
    case (pc, c) =>
      println(
        f"""PC: 0x${pc.toLong}%016x, Code Pos: ${getELFPos(pc.toLong)} count ${c}""".stripMargin)
  }
  generateGraph()
}

@main def TestLuaInterpreter_XS = {
  applyXiangShan2ndGenParam()
  test111body
}