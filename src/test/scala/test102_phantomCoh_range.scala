import HT.ASTNodes.{TLBEntryPermissionChangeNode, given_Conversion_ObjectValueNode_ArithNode, given_Conversion_ValueNode_ArithNode}
import HT.ASTUtils.printAST
import HT.AttackerZones.{SameProcess, SameProcessSwitch}
import HT.CachelineUtils.EvictSetFromL1DCacheline
import HT.ASTNodes.given_Conversion_Long_ValueNode
import HT.CodeGen.*
import HT.Executors.pyStr2Result
import HT.Permissions.Permission.{AttackerPublic, VictimPublic}
import HT.StdLib.*
import HT.Types.*
import HT.*
import HT.given_Conversion_ValueNode_PlacementOperator
import HT.ASTNodes.given_Conversion_Long_ArithNode
import HT.given_Conversion_Long_PlacementOperator
import HT.ASTNodes.given_Conversion_Int_ValueNode
import HT.ASTNodes.given_Conversion_Int_ArithNode
import HT.given_Conversion_Int_PlacementOperator
import HT.observation.*
import HT.simrunners.XSRun
import HT.Types.TLBPermission
import HT.observation.AbstractionLayer.*

def test_phantom_range_body(): Any = {

  val startTime = System.currentTimeMillis()

  val train_rep = 3
  val ITERATION = if (MarchParameters.ISA == "riscv64") 100 else 50000000L
  val attack: Boolean = true // may adjust to get baseline reference data
  val offset = if (attack) 0 else 0x400
  val PhantomOffset = MarchParameters.PageSize
  val probeDist = if (MarchParameters.ISA == "riscv64") 14 else 14

  val ast = Victim {
    val victim_jmp_dest = Label()
    val victim_jmp = Inst.ControlFlow() // ControlflowInst()
    val attacker_jmp_dest = Label()
    val attacker_jmp = ControlflowInst()

    val victim = Func(Bool)() {
      val retval = Bool(true)

      // victim should share BP history here
      Attacker() {
        FlushBPHistory()
      }
      Jmp(inst = victim_jmp,
        target = victim_jmp_dest)

      PlaceLabel(victim_jmp_dest)

      ret(retval)
    }

    Constrain() {
      NextDLine(victim_jmp, victim_jmp_dest)
    }

    Attacker() {
      val attack_prepare = Func(Bool)() {
        val retval = Bool(true)
        FlushBPHistory()
        Jmp(inst = attacker_jmp, target = attacker_jmp_dest)
        PlaceLabel(attacker_jmp_dest)
        ret(retval)
      }
    }

    Attacker() {
      val cost = UInt64(0, permission = AttackerPublic)
      val nextvar = MakeNVars(probeDist, UInt64(0))
    }

    val main = Func(SInt)() {
      Attacker() {
        val perm = TLBPermission(R = true, W = true, X = true)
        PagePermissionChange(refo("nextvar_0").page, perm)
      }
      Control() {
        val total = UInt64(0)
      }
      val i = UInt(0)
      While(i < (
        if (MarchParameters.ISA == "riscv64") ITERATION * 100 else ITERATION / 64
        )
      ) {
        Attacker() {
          // train BP history and jmp to a target
          val j = UInt(0)
          While(j < train_rep) {
            call("attack_prepare")();
            j := j + 1
          }
        }
        ICacheFlush()
        // victim code will touch the cacheline because of phantom
        victim()

        Attacker() {
          Mfence()
          Timing(refv("cost")) {
            (0 until probeDist).map {
              i => refv(s"nextvar_$i") := 1
            }
            Mfence()
          }
        }

        Control() {
          refv("total") := refv("total") + refv("cost")
        }
        i := i + 1
      }
      Control() {
        printInt(refv("total"))
      }

      MainRet(0)
    }

    if (MarchParameters.ISA == "x86_64") {
      Constrain() {
        AppendConstraint(attacker_jmp_dest.saddr === attacker_jmp.saddr + PhantomOffset)
        AppendConstraint(victim_jmp.saddr === 0x100000000L)
        Intel14GJmpCollisionPhantom(
          victim = victim_jmp,
          attacker = attacker_jmp
        )
        (1 until probeDist).map {
          i => AppendConstraint(refo(s"nextvar_${i}").page === refo(s"nextvar_${i - 1}").page)
        }
        (1 until probeDist).map {
          i => NextDLine(refo(s"nextvar_${i - 1}"), refo(s"nextvar_$i"))
        }
        AppendConstraint(refo("nextvar_0").saddr === victim_jmp.saddr + PhantomOffset + offset)
      }
    } else if (MarchParameters.MarchName == "XiangShanNanhu") {
      Constrain() {
        (1 until probeDist).map {
          i => NextDLine(refo(s"nextvar_${i - 1}"), refo(s"nextvar_$i"))
        }
        (1 until probeDist).map {
          i => AppendConstraint(refo(s"nextvar_${i}").page === refo(s"nextvar_${i - 1}").page)
        }
        AppendConstraint(attacker_jmp_dest.saddr === attacker_jmp.saddr + PhantomOffset)
        AppendConstraint(victim_jmp.saddr === 0x40000000L)
        XiangShanJmpCollisionPhantom(
          victim = victim_jmp,
          attacker = attacker_jmp
        )
        AppendConstraint(refo("nextvar_0").saddr === victim_jmp.saddr + PhantomOffset + offset)

      }
    } else {
      throw new Exception(s"Unsupported Platform" + MarchParameters.MarchName)
    }
  }
  GlobalPass(ast)
  if (MarchParameters.ISA == "riscv64") {


    println("Post CodeGen Current time cost: " + (System.currentTimeMillis() - startTime) + "ms")

    val victim_jmp = refo("victim_jmp")

    val victim_pc = observation.getPC(victim_jmp)
    println(f"PC address (hex): 0x$victim_pc%X")

    val victim_dest_pc = observation.getPC(refo("victim_jmp_dest"))
    println(f"victim dest address (hex): 0x$victim_dest_pc%X")

    val victim_poisoned = observation.getPC(refo("nextvar_0"))
    println(f"victim poisoned address (hex): 0x$victim_poisoned%X")

    val attack_pc = observation.getPC(refo("attacker_jmp"))
    println(f"attack src address (hex): 0x$attack_pc%X")

    val attack_dest = observation.getPC(refo("attacker_jmp_dest"))
    println(f"attack dest address (hex): 0x$attack_dest%X")

    val vcd_path = XSRun(100000L, dumpStartCycle = Some(0), dumpEndCycle = Some(100000))

    println("Post XS Run Current time cost: " + (System.currentTimeMillis() - startTime) + "ms")

    GlobalCycle = 0 // set the global iteration to 10000 to match the dump start cycle
    //val vcd_path = "/mnt/ssd4t/home/xim-intel14/phantom-range.vcd"

    val capt = observation.examples.branch_notifier_signals
    val parser = VCDParser(vcd_path.toString, XSBPUSignalSet ++ XSICacheSignalSet ++ XSFTQSignalSet ++ XSROBSignalSet)

    println(s"VCD path: $vcd_path, max time: ${parser.getMaxTime}")

    println("Post VCD parse, Current time cost: " + (System.currentTimeMillis() - startTime) + "ms")

    // step 1: check attacker has injected a malicious btb entry
    val bpuObj = new XSBPU(parser)
    val icacheObj = new XSICache(parser)
    val ftqObj = new XSFTQ(parser)
    val robObj = new XSROB(parser)

    val updateStart = () => {
      if (!bpuObj.updateValid(GlobalCycle)) {
        false
      } else {
        val lst = bpuObj.updateTargets(GlobalCycle)

        if (lst(1).isEmpty) {
          false // not what we expect
        } else {
          // println(f"debug updatePc ${bpuObj.updatePc(GlobalIte)}%X updateTargets: ${lst(1).get}%X updateBlockEnd ${bpuObj.updateBlockEnd(GlobalIte)}%X")
          bpuObj.withinUpdateRange(GlobalCycle, attack_pc) && lst(1).get == attack_dest
        }
      }
    }

    val predictStart = () => {
      if (!bpuObj.lastOutValid(GlobalCycle)) {
        false
      } else {
        bpuObj.lastOutWithinRange(GlobalCycle, victim_pc) && bpuObj.lastOutTarget(GlobalCycle) == victim_poisoned
      }
    }

    var predictFtqIdx: (BigInt, BigInt) = (0, 0)

    val predictRedirect = () => {
      ftqObj.hasRedirect(GlobalCycle) && ftqObj.redirectIsOlderThan(GlobalCycle, predictFtqIdx._1, predictFtqIdx._2, 0) && !(bpuObj.lastOutFtqPtr(GlobalCycle) == predictFtqIdx)
    }

    val predictCommit = () => {
      // we want to check that the prediction is commited
      robObj.commitHasFtqIdx(GlobalCycle, predictFtqIdx._1, predictFtqIdx._2)
    }

    val blockEnd = () => {
      predictRedirect() || predictCommit()
    }

    val predEndMeta = () => {
      if (predictRedirect()) {
        s"Phantom prediction redirected"
      } else {
        s"Phantom prediction commited"
      }
    }

    val fetchStart = () => {
      // here we want to check that after the poisoned prediction, we can have a corresponding fetch
      if (!icacheObj.prefetchReqFire(GlobalCycle)) {
        false
      } else {
        icacheObj.prefetchWithinRange(GlobalCycle, victim_poisoned)
      }
    }

    var poisonedPaddr: Option[BigInt] = None

    val tlbStart = () => {
      (icacheObj.TLB0FirePC(GlobalCycle, victim_poisoned) && !icacheObj.TLB0RespFault(GlobalCycle)) ||
        (icacheObj.TLB1FirePC(GlobalCycle, victim_poisoned) && !icacheObj.TLB1RespFault(GlobalCycle))
    }

    val tlbStartMeta = () => {
      f"TLB fired at cycle $GlobalCycle with poisoned Paddr: 0x${poisonedPaddr.get}%X"
    }

    val tlbFault0 = () => {
      icacheObj.TLB0FirePC(GlobalCycle, victim_poisoned) && icacheObj.TLB0RespFault(GlobalCycle)
    }

    val tlbFault1 = () => {
      icacheObj.TLB1FirePC(GlobalCycle, victim_poisoned) && icacheObj.TLB1RespFault(GlobalCycle)
    }

    val tlbFaultStart = () => {
      tlbFault0() || tlbFault1()
    }

    val tlbEnd = () => {
      tlbFaultStart() // we want to end the check if either TLB fault or another fetch request is made
    }

    val tlbFaultEnd = () => {
      tlbStart() // we want to end the check if either TLB request or another fetch request is made
    }

    val tlbFaultMeta = () => {
      if (tlbFault0()) {
        s"TLB Port 0 fault"
      } else {
        s"TLB Port 1 fault"
      }
    }

    var sourceId: Option[BigInt] = None

    val memAcquireStart = () => {
      // we want to check for a memory acquire request following the TLB translation
      if (poisonedPaddr.isEmpty) {
        throw new Exception("Poisoned Paddr is not set, TLB miss?")
      }
      icacheObj.memAcquireFireWithPAddress(GlobalCycle, poisonedPaddr.get)
    }

    val memAcquireMeta = () => {
      f"Memory acquire request for poisoned Paddr: 0x${poisonedPaddr.get}%X with sourceId: ${sourceId.getOrElse("unknown")} at cycle $GlobalCycle"
    }

    val memGrantStart = () => {
      icacheObj.memGrantValid(GlobalCycle) && icacheObj.memGrantSource(GlobalCycle) == sourceId.get
    }

    val predStartMeta = () => {
      s"prediction start at cycle $GlobalCycle with FtqPtr: (${predictFtqIdx._1} ${predictFtqIdx._2})"
    }

    setGraphRange(32000, 100000)

    On(updateStart, "BTB update") {
    }
    GlobalCycle = 0 // increment the global iteration to match the dump start cycle

    // step 2: check that the victim branch gets poisoned afterwards
    // the victim branch within range and predicted to poisoned target
    Range(predictStart, blockEnd, "BTB prediction") {
      entry{
        predictFtqIdx = bpuObj.lastOutFtqPtr(GlobalCycle)
        EventLog(predStartMeta())
        // step 3: check that poisoned target is being prefetched
        OnNextOnce(fetchStart, "Prefetch") {
          entry {
            OnNextOnceUnless(tlbStart, tlbEnd, "TLB Resp") {
              entry {
                if (icacheObj.TLB0FirePC(GlobalCycle, victim_poisoned)) {
                  poisonedPaddr = Some(icacheObj.TLB0FirePaddr(GlobalCycle))
                } else if (icacheObj.TLB1FirePC(GlobalCycle, victim_poisoned)) {
                  poisonedPaddr = Some(icacheObj.TLB1FirePaddr(GlobalCycle))
                }
                EventLog(tlbStartMeta())
                // step 4: check that memory request is sent downward
                rangeNextOnceUnless(memAcquireStart, memGrantStart, blockEnd, "Memory Transaction") {
                  entry {
                    sourceId = Some(icacheObj.memAcquireSource(GlobalCycle))
                    EventLog(memAcquireMeta())
                  }
                }
              }
            }
            OnNextOnceUnless(tlbFaultStart, tlbFaultEnd, "TLB Fault") {
              entry {
                EventLog(tlbFaultMeta())
              }
            }
          }
        }
      }
      exit {
        EventLog(predEndMeta())
      }
    }
    println(s"Post test Current time cost: " + (System.currentTimeMillis() - startTime) + "ms")
    generateGraph()
  }
}

@main def testVCDPhantomRange() = {
  applyXiangShan2ndGenParam()
  test_phantom_range_body()
}
@main def testVCDPhantomRange_IA() = {
  applyIntel14thGenParam() // apply to Intel 14/7th gen
  test_phantom_range_body()
}
// check test51_phantom-notfixed for the AMD Zen 4 implementation