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
import observation.AbstractionLayer.*
import HT.simrunners.XSRun

import scala.collection.mutable.ArrayBuffer

def test114body = {
  // val ELFPath = "/home/xim-intel14/ELF/riscv-rootfs/luainit.elf"
  val Gem5TracePath = "/root/gem5/gem5-trace.txt"
  //  "/mnt/ssd4t/home/xim-intel14/luahard.vcd" "/mnt/ssd4t/home/xim-intel14/lua-correlation.vcd"
  // val vcd_path = XSRun(1000000, customELF = Some(ELFPath)) // "/home/xim-intel14/XS///////build/2025-10-07@16:00:31.vcd" //
  val luaPath = "/root/gem5/lua"
  ELFParseInit(luaPath)
  println("Debug: fileNumbers: " + fileNumbers)
  val luaPC = getELFAddr("lvm.c:1665").head
  println(f"Inst that load Lua PC: 0x${luaPC}%08x")
  val parser = Gem5Parser(Gem5TracePath)

  val gem5Obj = Gem5O3(parser)

  setGraphRange(0, parser.getMaxTime.toInt)

  var commitMap = ArrayBuffer[(Int, Long)]() // array of pairs (cycle, pc)

  val commitCond = () => {
    gem5Obj.commitWithPC(GlobalCycle, luaPC)
  }

  On(commitCond, "Lua PC inst commit") {
    entry{
      val luaPC = gem5Obj.getRax(GlobalCycle)
      commitMap += ((GlobalCycle, luaPC.toLong))
    }
  }

  println("Commit Map (Cycle, Lua PC):")
  commitMap.foreach { case (cycle, pc) =>
    println(f"Cycle: $cycle, Lua PC: ${pc}")
  }

  GlobalCycle = 0 // reset cycle for next analysis

  var mispredMap = Map[BigInt, Int]()

  val mispredCond = () => {
    gem5Obj.hasMispred(GlobalCycle)
  }

  On(mispredCond, "mispredictions") {
    entry{
      val pc = gem5Obj.MispredPC(GlobalCycle)
      mispredMap += (pc -> (mispredMap.getOrElse(pc, 0) + 1))
      EventLog(f"Mispred PC: 0x${pc.toLong}%X Lua PC: 0x${lookupClosestLessThan(GlobalCycle, commitMap)}%08x at cycle $GlobalCycle")
    }
  }
  println("Misprediction Map:")
  mispredMap.toList.sortBy(-_._2).foreach { case (pc, count) =>
    println(f"PC: 0x${pc.toLong}%016x, Count: $count, ELF Pos: ${getELFPos(pc.toLong)}")
  }
  generateGraph()

  // test abstraction layer
  /*
  GlobalCycle = 0
  val dcacheReqCond = () => {
    gem5Obj.hasDCacheReq(GlobalCycle)
  }

  On(dcacheReqCond, "DCache request") {
    entry{
      val reqAddr = gem5Obj.getDCacheReqAddr(GlobalCycle)
      EventLog(f"DCache request at cycle $GlobalCycle vaddr: 0x${reqAddr.head.toLong}%X")
    }
  }

  GlobalCycle = 0
  val predCond = () => {
    gem5Obj.hasPred(GlobalCycle)
  }

  On(predCond, "predictions") {
    entry{
      val pc = gem5Obj.getPredPC(GlobalCycle)
      val taken = gem5Obj.getPredTaken(GlobalCycle)
      val target = gem5Obj.getPredTarget(GlobalCycle)
      EventLog(f"Pred PC: 0x${pc.toLong}%X at cycle $GlobalCycle taken: $taken target: 0x${target.toLong}%X")
    }
  }

  GlobalCycle = 0
  val fetchReqCond = () => {
    gem5Obj.hasFetch(GlobalCycle)
  }
  On(fetchReqCond, "Fetch request") {
    entry {
      val reqPC = gem5Obj.getFetchPC(GlobalCycle)
      EventLog(f"Fetch request at cycle $GlobalCycle for PC: 0x${reqPC.head}%X")
    }
  }

  GlobalCycle = 0
  val icacheReqCond = () => {
    gem5Obj.hasICacheReq(GlobalCycle)
  }

  On(icacheReqCond, "ICache request") {
    entry{
      val reqPC = gem5Obj.getICacheReqAddr(GlobalCycle)
      EventLog(f"ICache request at cycle $GlobalCycle for PC: 0x${reqPC.toLong}%X")
    }
  }
  */

}

@main def TestLuaInterpreter_Gem5 = {
  // applyXiangShan2ndGenParam()
  test114body
}