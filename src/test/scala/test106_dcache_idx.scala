import HT.ASTUtils.printAST
import HT.{Attacker, Constrain, Control, ExactPadding, Func, GlobalPass, GlobalSolve, If, Jmp, Label, Load, MarchParameters, Mfence, Padding, PlaceLabel, SMTCode, Timing, Victim, While, XiangShan2ndGenParam, a, applyIntel14thGenParam, applyXiangShan2ndGenParam, call, codeGen, given_Conversion_ValueNode_PlacementOperator, imm, outputGen, placement, printInt, printSMT, refo, refv, ret, tryRun}
import HT.CodeGen.*
import HT.StdLib.{Cacheline2Var, DCacheFlush, DCachePtrFlush, FlushBPHistory, InlineAsm, MainRet, PtrLoad, SyscallSwitch, USleepSwitch, Var2Ptr}
import HT.Types.{Bool, Cacheline, ControlflowInst, MakeNVars, SInt, UInt, UInt64, types}
import HT.ASTNodes.{AsmOperand, ValueNode, given_Conversion_Int_ArithNode, given_Conversion_Int_ValueNode, given_Conversion_Long_ValueNode, given_Conversion_ObjectValueNode_ArithNode, given_Conversion_ValueNode_ArithNode}
import HT.CachelineUtils.EvictSetFromL1DCacheline
import HT.Executors.pyStr2Result
import HT.Permissions.Permission.VictimPublic
import HT.given_Conversion_Int_PlacementOperator
import HT.given_Conversion_Long_PlacementOperator
import HT.*
import HT.observation.*
import HT.simrunners.{XSRun, emu_path}
import HT.observation.AbstractionLayer.*

def test106body = {

  val opted = false

  val ast = Victim{ // : List[ValueNode]
    val probe_vars: List[ValueNode] = MakeNVars(MarchParameters.L1DWay + 1, UInt64(0))
    val main = Func(SInt)() {
      val i: ValueNode = UInt64(0)
      While(i < 30) {
        (0 to MarchParameters.L1DWay).foreach { l =>
          Store(1, probe_vars(l))
        }
        i := i + 1
      }
      MainRet(0) // can be Ret(0)
    }
    Constrain(){
      /*
      AppendConstraint(probe_vars.head.obj.saddr === 0x40000000) // stronger constraint
      (1 to MarchParameters.L1DWay).foreach { i =>
        //AppendConstraint(UniqueLines(probe_var(i).obj.saddr, probe_var(i-1).obj.saddr))
        //AppendConstraint(SetIndexOld(probe_var(i).obj.saddr) === SetIndexOld(probe_var(i-1).obj.saddr))
        AppendConstraint(probe_vars(i).obj.saddr === probe_vars(i - 1).obj.saddr + (MarchParameters.L1DLine * MarchParameters.L1DSet)) // stronger constraint
      }
      */
      probe_vars.combinations(2).foreach {
        case Seq(a,b) =>
          AppendConstraint(UniqueLines(a.obj.saddr, b.obj.saddr))
          AppendConstraint(SetIndexOld(a.obj.saddr) === SetIndexOld(b.obj.saddr))
      }
      AppendConstraint(SetIndexNew(probe_vars.head.obj.saddr) =/= SetIndexNew(probe_vars.last.obj.saddr))
    }
  }

  GlobalPass(ast)
  if (opted) {
    emu_path = "emu-dcache-opt"
  } else {
    emu_path = "emu-dcache-base"
  }

  val vcd_path = XSRun(700000L)
  //val vcd_path = "/home/xim-intel14/XS/build/2026-03-10@15:20:13.vcd"
  //val vcd_path = if (opted) "/mnt/ssd4t/home/xim-intel14/dcache-idx.vcd" else "/mnt/ssd4t/home/xim-intel14/dcache-idx-base.vcd"

  val storeAddrs = (0 to MarchParameters.L1DWay).map { i =>
    observation.getPC(refo("probe_vars", i)) // -> probe_var, i
  }

  val parser = VCDParser(vcd_path.toString, XSDCacheSignalSet ++ XSIPrefetchSignalSet)

  println(s"VCD path: $vcd_path, max time: ${parser.getMaxTime}")

  val dcacheObj = new XSDCache(parser)
  val iprefetchObj = new XSIPrefetch(parser)


  var totalEvictions: Int = 0

  def reqInDCache(): Boolean = {
    dcacheObj.reqValid(GlobalCycle) && storeAddrs.contains(dcacheObj.reqVaddr(GlobalCycle))
  }

  def isOSRange(n: BigInt): Boolean = {
    // define the OS memory allocation range, this is just an example and should be adjusted according to the actual OS behavior
    n >= 0x3ffff00000000L
  }

  var inOSRange = 0
  def reqStartOS(): Boolean = {
    inOSRange < GlobalCycle && iprefetchObj.prefetchReqFire(GlobalCycle) && isOSRange(iprefetchObj.prefetchReqVAddr(GlobalCycle))
  }

  def reqLeaveOS(): Boolean = {
    iprefetchObj.prefetchReqFire(GlobalCycle) && !isOSRange(iprefetchObj.prefetchReqVAddr(GlobalCycle))
  }

  setGraphRange(28000, 68000)

  val traceOS = false

  if (!traceOS) {
    On(reqInDCache, "storing request in DCache") {
      entry {
        if (dcacheObj.reqNeedsEviction(GlobalCycle)) {
          totalEvictions += 1
          EventLog(f"Eviction in DCache for vaddr")
        } else {
          EventLog("No eviction")
        }
      }
    }
  } else {
    var evictionCount: Long = 0
    // trace instruction prefetch evictions in ICache, caused by OS memory allocation
    Range(reqStartOS, reqLeaveOS, "OS range prefetch") {
      entry {
        inOSRange = GlobalCycle
        EventLog(f"Prefetch request enters OS range at cycle: ${GlobalCycle}")
      }
      inRange{
        if (iprefetchObj.prefetchReqFire(GlobalCycle)) {
          inOSRange = GlobalCycle
          evictionCount += (if (iprefetchObj.prefetchMiss0(GlobalCycle)) 1 else 0)
          evictionCount += (if (iprefetchObj.prefetchDoubleline(GlobalCycle) && iprefetchObj.prefetchMiss1(GlobalCycle)) 1 else 0)
        }
      }
      exit {
        EventLog(f"Prefetch request leaves OS range at cycle: ${GlobalCycle}, total eviction count until this range: ${evictionCount}")
      }
    }
  }


  println(s"Total eviction in DCache: $totalEvictions")
  generateGraph()
}

@main def TestDCacheIdx_XS = {
  applyXiangShan2ndGenParam()
  test106body
}
