import HT.ASTUtils.printAST
import HT.{Attacker, Constrain, Control, ExactPadding, Func, GlobalPass, GlobalSolve, If, Jmp, Label, Load, MarchParameters, Mfence, Padding, PlaceLabel, SMTCode, Timing, Victim, While, XiangShan2ndGenParam, a, applyIntel14thGenParam, applyXiangShan2ndGenParam, call, codeGen, given_Conversion_ValueNode_PlacementOperator, imm, outputGen, placement, printInt, printSMT, refo, refv, ret, tryRun}
import HT.CodeGen.*
import HT.StdLib.{Cacheline2Var, DCacheFlush, DCachePtrFlush, FlushBPHistory, MainRet, PtrLoad, SyscallSwitch, USleepSwitch, Var2Ptr}
import HT.Types.{Bool, Cacheline, ControlflowInst, Inst, SInt, UInt, UInt64, types}
import HT.ASTNodes.given_Conversion_ObjectValueNode_ArithNode
import HT.ASTNodes.given_Conversion_ValueNode_ArithNode
import HT.CachelineUtils.EvictSetFromL1DCacheline
import HT.Executors.pyStr2Result
import HT.Permissions.Permission.VictimPublic
import HT.ASTNodes.given_Conversion_Int_ValueNode
import HT.ASTNodes.given_Conversion_Int_ArithNode
import HT.given_Conversion_Int_PlacementOperator
import HT.ASTNodes.given_Conversion_Long_ValueNode
import HT.given_Conversion_Long_PlacementOperator
import HT.*
import HT.observation.*
import HT.simrunners.{XSRun, emu_path}
import HT.observation.AbstractionLayer.*

def test104body = {
  val startTime = System.currentTimeMillis()

  val attack: Boolean = true
  val mitigation: Boolean = false

  if (mitigation) {
    emu_path = "emu-half"
  }

  val ast = Victim {
    val victim_jmp = Inst.ControlFlow()
    val attack_jmp = Inst.ControlFlow()
    val spectre_var = UInt64(0)
    val base_var = UInt64(0)

    val cond0 = Bool(true)
    val cond1 = Bool(true)

    val victim = Func(Bool)() {
      val retval = Bool(true)

      // victim should share BP history here
      FlushBPHistory()

      If (cond0 === cond1, victim_jmp) {
        base_var := 1
      } .Else {
        spectre_var := spectre_var + 1
      }
      ret(retval)
    }

    Constrain() {
      AppendConstraint(spectre_var.obj.saddr === 0x41001040)
      AppendConstraint(cond0.obj.saddr === 0x41000000)
      FarAway(spectre_var.obj, base_var.obj)
      NextDLine(cond0.obj, cond1.obj)
    }

    Attacker() {
      val a1 = UInt64(0)
      val a2 = UInt64(0)
      val bool1 = Bool(true)
      val bool2 = Bool(false)

      val attack_prepare = Func(Bool)() {
        val retval = Bool(true)
        FlushBPHistory()
        If (bool1 === bool2, attack_jmp) {
          a1 := imm(1)
        } .Else {
          a2 := a2 + imm(1)
        }
        ret(retval)
      }

      val tmp = UInt64(0)
    }

    val main = Func(SInt)() {
      val cost = UInt64(0, permission = VictimPublic)
      val total = UInt64(0)
      val i = UInt(0)

      victim()
      cond0 := 0 // XiangShan FTB does not record branches that never taken
      victim()
      cond0 := 1
      cond1 := 1

      Attacker() {
        refv("bool1") := 0
        refv("bool2") := 0
        call("attack_prepare")() // do not jump, always taken bit will not be set for our conditional br
        refv("bool1") := 1
        call("attack_prepare")() // let FTB recognize our cond br
        refv("bool1") := 0
        call("attack_prepare")() // clear always taken bit by not taken once
        refv("bool1") := 1 // resume to taken
      }

      While(i < (if (MarchParameters.ISA == "x86_64") 10000 else 0x100)) {
        Attacker() {
          // train BP history and jmp to a target
          val j = UInt(0)
          While(j < 3) {
            if (attack)
              call("attack_prepare")();
            j := j + 1
          }
          val spectre_ptr = Var2Ptr(refv("spectre_var"))
          DCachePtrFlush(spectre_ptr)
          val cond0_ptr = Var2Ptr(refv("cond0"))
          DCachePtrFlush(cond0_ptr)
          val cond1_ptr = Var2Ptr(refv("cond1"))
          DCachePtrFlush(cond1_ptr)
          refv("tmp") := 1 // load data into DCache
          Mfence()
        }
        // victim code will touch the cacheline because of phantom
        victim()

        Attacker() {
          Mfence()
          Timing(cost) {
            Mfence()
            // if spectre execution, spectre_var will get loaded by victim()
            PtrLoad(refv("spectre_ptr"), refv("tmp"))
            Mfence()
          }
        }
        Control(){
          If (cost < 2000) {
            total := total + cost
          }
        }
        refv("i") := refv("i") + 1
      }

      printInt(total)

      MainRet(0)
    }

    if (MarchParameters.ISA == "x86_64") {
      /*
      Constrain() { // for intel 14th gen
        refo("spectre_var").dcacheline === refo("base_var").dcacheline - 100
        refo("spectre_var").saddr === 0x41001040
        refo("attack_jmp").saddr === 0x100000000L
        refo("victim_jmp").saddr === 0x300000000L // collide with attack jmp in TAGE but not essentially BTB
        refo("cond0").dcacheline === refo("cond1").dcacheline - 1
        refo("cond0").saddr === 0x41000000
      }*/
      Constrain() { // for amd, also works for intel 14th gen
        AppendConstraint(victim_jmp.saddr === 0x200000000000L) // collide with attack jmp in TAGE but not essentially BTB
        AMD64CondBrCollision(victim = victim_jmp, attacker = attack_jmp)
      }
    } else {
      Constrain() {
        AppendConstraint(victim_jmp.saddr === 0x80000000L) // collide with attack jmp in TAGE but not BTB
        XiangShanCondBrCollision(victim_jmp, attack_jmp)
      }
    }
  }
  GlobalPass(ast)

  println("Post CodeGen Current Time Cost: " + (System.currentTimeMillis() - startTime) + " ms")

  if (MarchParameters.ISA == "riscv64") {
    val vcd_path = XSRun(100000L, dumpStartCycle = Some(50000), dumpEndCycle = Some(100000))

    println("Post XSRun Current Time Cost: " + (System.currentTimeMillis() - startTime) + " ms")
    // val vcd_path = if (mitigation) "/mnt/ssd4t/home/xim-intel14/spectre-mitigation.vcd" else "/mnt/ssd4t/home/xim-intel14/spectre.vcd"
    GlobalCycle = 50000

    val parser = VCDParser(vcd_path.toString, XSBPUSignalSet ++ XSIbufferSignalSet ++ XSDCacheSignalSet ++ XSROBSignalSet ++ XSFTQSignalSet)

    println(s"VCD path: $vcd_path, max time: ${parser.getMaxTime}")

    println("Post VCDParser Current Time Cost: " + (System.currentTimeMillis() - startTime) + " ms")

    val attack_jmp = refo("attack_jmp")
    val victim_jmp = refo("victim_jmp")
    val spectre_var = refo("spectre_var")

    val attack_jmp_pc = observation.getPC(attack_jmp)
    val victim_jmp_pc = observation.getPC(victim_jmp)
    val spectre_var_addr = observation.getPC(spectre_var)

    println(f"Attack jump PC address (hex): 0x$attack_jmp_pc%X")
    println(f"Victim jump PC address (hex): 0x$victim_jmp_pc%X")

    val bpuObj = new XSBPU(parser)
    val ibufObj = new XSIBuffer(parser)
    val dcacheObj = new XSDCache(parser)
    val robObj = new XSROB(parser)
    val ftqObj = new XSFTQ(parser)

    var victimFTQPtr: (BigInt, BigInt) = (0, 0)
    var victimFTQOffset: BigInt = 0

    val attackUpdate = () => {
      bpuObj.updateValid(GlobalCycle) && bpuObj.updateTaken(GlobalCycle) && bpuObj.updateTakenPC(GlobalCycle) == attack_jmp_pc
    }

    val nextAttackUpdate = () => {
      bpuObj.updateValid(GlobalCycle + 1) && bpuObj.updateTaken(GlobalCycle + 1) && bpuObj.updateTakenPC(GlobalCycle + 1) == attack_jmp_pc
    }

    val victimPredStart = () => {
      // victim branch predict to taken
      val slot = ibufObj.PCWithinValid(GlobalCycle, victim_jmp_pc)
      slot >= 0 && ibufObj.getPredTaken(GlobalCycle, slot)
    }


    val victimRedirect = () => {
      // as we have got a wrong prediction, victim must have a redirect
      if (ftqObj.hasRedirect(GlobalCycle)) {
        // victim branch redirect
        val t = ftqObj.redirectFTQPtr(GlobalCycle)
        val o = ftqObj.redirectFTQOffset(GlobalCycle)
        t._1 == victimFTQPtr._1 && t._2 == victimFTQPtr._2 && o == victimFTQOffset
      } else {
        false
      }
    }

    val victimDCacheReq = () => {
      // victim speculatively sends a dcache request
      dcacheObj.loadMatchVaddr(GlobalCycle, spectre_var_addr) >= 0
    }

    var victimPaddr: BigInt = 0

    val victimMissReq = () => {
      dcacheObj.missReqFire(GlobalCycle) && dcacheObj.missReqMatchVaddr(GlobalCycle, spectre_var_addr)
    }

    val victimRefillReq = () => {
      dcacheObj.refillReqFire(GlobalCycle) && dcacheObj.refillReqMatchPaddr(GlobalCycle, victimPaddr)
    }

    val victimCommit = () => {
      // victim branch commit
      robObj.getValidPCs(GlobalCycle).contains(victim_jmp_pc)
    }

    val refillMeta = () => {
      // the refill meta is FTQ refilled with paddr
      s"Victim speculative refill request at cycle ${GlobalCycle} Paddr: 0x${victimPaddr.toString(16)}"
    }

    val victimPredStartMeta = () => {
      s"FTQPtr: (${victimFTQPtr._1} ${victimFTQPtr._2}) FTQOffset: $victimFTQOffset"
    }

    val victimMissStartMeta = () => {
      s"Victim speculative miss Paddr: 0x${dcacheObj.missReqPaddr(GlobalCycle).toString(16)}"
    }

    setGraphRange(80000, 100000)

    // step 1: attack inject taken prediction into BPU
    On(attackUpdate, "attack update predictors") {
      entry {
        // step 2: victim got the prediction
        rangeNextUnless(victimPredStart, victimCommit, nextAttackUpdate, "victim execution") {
          entry {
            val slot = ibufObj.PCWithinValid(GlobalCycle, victim_jmp_pc)
            victimFTQPtr = ibufObj.getFTQPtr(GlobalCycle, slot)
            victimFTQOffset = ibufObj.getFTQOffset(GlobalCycle, slot)
            // step 3: speculative flow trigger dcache request
            OnNextOnceUnless(victimDCacheReq, victimRedirect, "victim speculative data cache req") {
              entry {
                // step 4: if we see a miss request from mainPipe
                rangeNextOnceUnless(victimMissReq, victimRefillReq, victimRedirect, "victim speculative miss handle") {
                  // step 5: if we see a refill
                  entry {
                    victimPaddr = dcacheObj.missReqPaddr(GlobalCycle)
                    EventLog(victimMissStartMeta())
                  }
                  exit {
                    EventLog(refillMeta())
                  }
                }
              }
            }
            EventLog(victimPredStartMeta())
          }
        }
      }


    }

    println(s"Post till Current Time Cost: " + (System.currentTimeMillis() - startTime) + " ms")
    generateGraph()
  }
}


@main def TestSpectreRange_XS = {
  applyXiangShan2ndGenParam()
  test104body
}

@main def TestSpectreRange_IA = {
  applyIntel14thGenParam() // also works for intel 7th gen
  test104body
}

@main def TestSpectreRange_AMD = {
  applyAMDZen4Param()
  test104body
}