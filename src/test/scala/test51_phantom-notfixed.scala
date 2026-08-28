import HT.ASTUtils.printAST
import HT.{AppendConstraint, Attacker, Constrain, Control, ExactPadding, Func, GlobalPass, GlobalSolve, Intel14GInstrEvictionSet, Intel14GJmpCollisionPhantom, Jmp, Label, MarchParameters, Padding, PlaceLabel, SMTCode, Timing, Victim, While, XiangShan2ndGenParam, a, applyAMDZen4Param, applyIntel14thGenParam, applyIntel7thGenParam, applyXiangShan2ndGenParam, call, codeGen, given_Conversion_Int_PlacementOperator, given_Conversion_Long_PlacementOperator, given_Conversion_ValueNode_PlacementOperator, imm, outputGen, placement, printInt, printSMT, refo, refv, ret, tryRun, If}
import HT.CodeGen.*
import HT.StdLib.{Cacheline2Var, FlushBPHistory, MainRet, SyscallSwitch, USleepSwitch}
import HT.Types.{Bool, Cacheline, ControlflowInst, Inst, UInt, UInt64, types}
import HT.ASTNodes.given_Conversion_ObjectValueNode_ArithNode
import HT.ASTNodes.given_Conversion_ValueNode_ArithNode
import HT.CachelineUtils.EvictSetFromL1DCacheline
import HT.Executors.pyStr2Result
import HT.Permissions.Permission.{AttackerPublic, VictimPublic}
import HT.ASTNodes.given_Conversion_Int_ValueNode
import HT.ASTNodes.given_Conversion_Int_ArithNode
import HT.ASTNodes.given_Conversion_Long_ValueNode
import HT.StdLib.ICacheProbe

def test51body = {
  val train_rep = 3
  val attack: Boolean = true // may adjust to get baseline reference data
  val offset = if (attack) 0 else -0x100
  val PhantomOffset = MarchParameters.PageSize

  val ast = Victim {

    val victim_jmp_dest = Label()
    val victim_jmp = Inst.ControlFlow() // ControlflowInst()

    val victim = Func(Bool)() {
      val retval = Bool(true)

      // victim should share BP history here
      /*
      Attacker() {
        FlushBPHistory()
      }
      */
      Jmp(inst = victim_jmp,
          target = victim_jmp_dest)

      PlaceLabel(victim_jmp_dest)

      ret(retval)
    }

    Constrain() {
      AppendConstraint(refo("victim_jmp").saddr % MarchParameters.PageSize === 0)
      AppendConstraint(refo("victim_jmp_dest").saddr - refo("victim_jmp").saddr === 0x40) // just next line
    }

    Attacker() {
      val attacker_jmp_dest = Label()
      val attack_jmp = ControlflowInst()

      val attack_prepare = Func(types.Bool)() {
        val retval = Bool(true)
        // FlushBPHistory()
        Jmp(inst = attack_jmp, target = attacker_jmp_dest)
        PlaceLabel(attacker_jmp_dest)
        ret(retval)
      }
    }

    Attacker() {
      val victim_phantom = Func(types.Bool)() {
        val retval = Bool(true)
        ret(retval)
      }
    }

    val main = Func(types.SInt)() {
      Control(){
        val total = UInt64(0)
      }
      val i = UInt(0)
      While(refv("i") < 0x1000000) {
        Attacker() {
          // train BP history and jmp to a target
          val j = UInt(0)
          While(j < train_rep) {
            call("attack_prepare")()
            j := j + 1
          }

          // prime
          ICacheProbe(refo("victim_jmp"), (PhantomOffset + offset))
        }

        // victim code will touch the cacheline because of phantom
        victim()

        Attacker() {
          val cost = UInt64(0, permission = AttackerPublic)
          Timing(cost) {
            // probe
            ICacheProbe(victim_jmp, (PhantomOffset + offset))
          }
        }
        Control(){
          If (refv("cost") > 0) {
            refv("total") := refv("total") + refv("cost")
          }
        }
        i := i + 1
      }
      Control(){
        printInt(refv("total"))
      }

      MainRet(0)
    }

    if (MarchParameters.ISA == "x86_64") {
      Constrain() {
        AppendConstraint(refo("attacker_jmp_dest").saddr === refo("attack_jmp").saddr + PhantomOffset)
        AppendConstraint(refo("victim_jmp").saddr === 0x100000000L)
        Intel14GJmpCollisionPhantom(
          victim = refo("victim_jmp"),
          attacker = refo("attack_jmp")
        )
        AppendConstraint(refo("victim_phantom").saddr === refo("victim_jmp").saddr + PhantomOffset)
      }
    } else if (MarchParameters.MarchName == "XiangShanNanhu") {
      Constrain() {
        AppendConstraint(refo("attack_jmp").saddr === 0x60000000)
        AppendConstraint(refo("attack_jmp").saddr % MarchParameters.PageSize === 0)
        AppendConstraint(refo("attacker_jmp_dest").saddr - refo("attack_jmp").saddr === MarchParameters.PageSize)
        AppendConstraint(refo("victim_jmp").saddr === 0x40000000)
        AppendConstraint(refo("victim_phantom").saddr === refo("victim_jmp").saddr + MarchParameters.PageSize)
      }
    } else {
      throw new Exception(s"Unsupported Platform" + MarchParameters.MarchName)
    }
  }
  GlobalPass(ast)
}

@main def TestPhantom_IA = {
  //applyAMDZen5Param()
  applyIntel7thGenParam() // applies also to Intel 14
  test51body
}

@main def TestPhantom_AMD = {
  applyAMDZen4Param()
  test51body
}

@main def TestPhantom_XS = {
  applyXiangShan2ndGenParam()
  test51body
}