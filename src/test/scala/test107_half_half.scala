import HT.ASTUtils.printAST
import HT.{Attacker, Constrain, Control, ExactPadding, Func, GlobalPass, GlobalSolve, If, Jmp, Label, Load, MarchParameters, Mfence, Padding, PlaceLabel, SMTCode, Timing, Victim, While, XiangShan2ndGenParam, a, applyIntel14thGenParam, applyXiangShan2ndGenParam, call, codeGen, given_Conversion_ValueNode_PlacementOperator, imm, outputGen, placement, printInt, printSMT, refo, refv, ret, tryRun}
import HT.CodeGen.*
import HT.StdLib.{Cacheline2Var, DCacheFlush, DCachePtrFlush, FlushBPHistory, MainRet, PtrLoad, SyscallSwitch, USleepSwitch, Var2Ptr}
import HT.Types.{Bool, Cacheline, ControlflowInst, SInt, UInt, UInt64, types}
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

import scala.collection.mutable.ArrayBuffer

def test107body = {
  val attack: Boolean = true
  val mitigation: Boolean = true

  if (mitigation) {
    emu_path = "emu-half"
  }

  val ast = Victim {
    val victim_jmp = ControlflowInst()
    val attack_jmp = ControlflowInst()
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

    val test = Func(Bool)() {
      val retval = 0;
      ret(retval)
    }

    val main = Func(SInt)() {
      val cost = UInt64(0, permission = VictimPublic)
      val total = UInt64(0)
      val i = UInt(0)

      victim()
      cond0 := 0 // specific for XiangShan: XiangShan FTB does not record branches that never taken
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

      While(i < (if (MarchParameters.ISA == "x86_64") 100 else 0x100)) {
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
          test()
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

  val vcd_path = XSRun(50000L, dumpStartCycle = Some(50000), dumpEndCycle = Some(100000))
  // val vcd_path = if (mitigation) "/mnt/ssd4t/home/xim-intel14/spectre-mitigation.vcd" else "/mnt/ssd4t/home/xim-intel14/spectre.vcd"
  GlobalCycle = 50000

  val parser = VCDParser(vcd_path.toString, XSBPUSignalSet ++ XSTAGESignalSet)

  println(s"VCD path: $vcd_path, max time: ${parser.getMaxTime}")

  val attack_jmp = refo("attack_jmp")
  val victim_jmp = refo("victim_jmp")
  val spectre_var = refo("spectre_var")

  val attack_jmp_pc = observation.getPC(attack_jmp)
  val victim_jmp_pc = observation.getPC(victim_jmp)
  val spectre_var_addr = observation.getPC(spectre_var)

  println(f"Attack jump PC address (hex): 0x$attack_jmp_pc%X")
  println(f"Victim jump PC address (hex): 0x$victim_jmp_pc%X")

  val bpuObj = new XSBPU(parser)
  val tageObj = new XSTAGE(parser)

  val attackTableUpdate = () => {
    bpuObj.updateValid(GlobalCycle - 2) && bpuObj.updateWithinRange(GlobalCycle - 2, attack_jmp_pc) &&
      (0 until XSTAGETableCount).map{
        i => tageObj.updateValid(GlobalCycle, i)
      }.reduce(_ || _)
  }

  val victimTableUpdate = () => {
    bpuObj.updateValid(GlobalCycle - 2) && bpuObj.updateWithinRange(GlobalCycle - 2, victim_jmp_pc) &&
      (0 until XSTAGETableCount).map{
        i => tageObj.updateValid(GlobalCycle, i)
      }.reduce(_ || _)
  }
  var attackTag: ArrayBuffer[BigInt] = ArrayBuffer((0 until XSTAGETableCount).map(_ => -1): _*)
  var attackIdx: ArrayBuffer[BigInt] = ArrayBuffer((0 until XSTAGETableCount).map(_ => -1): _*)
  var attackGHist: ArrayBuffer[BigInt] = ArrayBuffer((0 until XSTAGETableCount).map(_ => -1): _*)

  var victimTag: ArrayBuffer[BigInt] = ArrayBuffer((0 until XSTAGETableCount).map(_ => -1): _*)
  var victimIdx: ArrayBuffer[BigInt] = ArrayBuffer((0 until XSTAGETableCount).map(_ => -1): _*)
  var victimGHist: ArrayBuffer[BigInt] = ArrayBuffer((0 until XSTAGETableCount).map(_ => -1): _*)


  val victimStartInfo = () => {
    val updateTable = tageObj.updateValidPos(GlobalCycle)
    s"Victim update at GlobalIte: ${GlobalCycle} table: ${updateTable} tag: ${victimTag(updateTable)} idx: ${victimIdx(updateTable)} ghist: ${victimGHist(updateTable)}"
  }

  val attackStartInfo = () => {
    val updateTable = tageObj.updateValidPos(GlobalCycle)
    s"Attack update at GlobalIte: ${GlobalCycle} table: ${updateTable} tag: ${attackTag(updateTable)} idx: ${attackIdx(updateTable)} ghist: ${attackGHist(updateTable)}"
  }

  setGraphRange(50000, 100000)


  // step 1: attack update
  On(attackTableUpdate, "Attack update") {
    entry {
      // record update ghist, idx and tag
      val updateTable = tageObj.updateValidPos(GlobalCycle)
      attackTag(updateTable) = tageObj.getUpdateTag(GlobalCycle, updateTable)
      attackIdx(updateTable) = tageObj.getUpdateIdx(GlobalCycle, updateTable)
      attackGHist(updateTable) = tageObj.getUpdateGHist(GlobalCycle, updateTable)
      EventLog(attackStartInfo())
    }
  }

  GlobalCycle = 50000

  On(victimTableUpdate, "Victim update") {
    entry {
      // record update ghist, idx and tag
      val updateTable = tageObj.updateValidPos(GlobalCycle)
      victimTag(updateTable) = tageObj.getUpdateTag(GlobalCycle, updateTable)
      victimIdx(updateTable) = tageObj.getUpdateIdx(GlobalCycle, updateTable)
      victimGHist(updateTable) = tageObj.getUpdateGHist(GlobalCycle, updateTable)
      EventLog(victimStartInfo())
    }
  }

  (0 until XSTAGETableCount).foreach { i =>
    if (attackTag(i) != -1 && victimTag(i) != -1) {
      // valid record
      if (attackTag(i) == victimTag(i) && (attackIdx(i) ^ victimIdx(i)) == 2 && attackGHist(i) == victimGHist(i)) {
        // valid common record
        println(s"Table $i: Attack and Victim updates match! Tag: ${attackTag(i)}, Index: ${attackIdx(i)}, GHist: ${attackGHist(i)}")
      } else {
        // valid but different record
        println(s"Table $i: Attack and Victim updates do not match! Attack - Tag: ${attackTag(i)}, Index: ${attackIdx(i)}, GHist: ${attackGHist(i)}; Victim - Tag: ${victimTag(i)}, Index: ${victimIdx(i)}, GHist: ${victimGHist(i)}")
      }
    } else {
      // invalid common record
    }
  }


  generateGraph()

}


@main def TestSpectreHalfHalf_XS = {
  applyXiangShan2ndGenParam()
  test107body
}
