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

def test_vcd_phantom_body() = {

  val train_rep = 3
  val ITERATION = if (MarchParameters.ISA == "riscv64") 100 else 50000000L
  val Attack: Boolean = true // may adjust to get baseline reference data
  val offset = if (Attack) 0 else 0x400
  val PhantomOffset = MarchParameters.PageSize
  val probeDist = if (MarchParameters.ISA == "riscv64") 14 else 14

  val ast = Victim {
    val victim_jmp_dest = Label()
    val victim_jmp = ControlflowInst()
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
  val victim_jmp = refo("victim_jmp")

  val victim_pc = observation.getPC(victim_jmp)
  println(f"PC address (hex): 0x$victim_pc%X")

  val victim_dest_pc = observation.getPC(refo("victim_jmp_dest"))
  println(f"victim dest address (hex): 0x$victim_dest_pc%X")

  val attck_pc = observation.getPC(refo("attacker_jmp"))
  println(f"attack src address (hex): 0x$attck_pc%X")

  val attck_dest = observation.getPC(refo("attacker_jmp_dest"))
  println(f"attack dest address (hex): 0x$attck_dest%X")

  val vcd_path = XSRun(20000L)
  val capt = observation.examples.branch_notifier_signals
  //val parser = VCDParserLimitedEfficient("/home/sergi/testvcd.vcd", capt)
  val parser = VCDParser(vcd_path.toString, capt)

  val b_follower = observation.examples.BranchNotifier(parser)

  //val branch_results = b_follower.parse()
  for br <- b_follower.parse() do {
    if (br.source_pc == victim_pc) then {
      print(f"[${br.time}ns] branch pc: 0x${br.source_pc}%x, branch target: 0x${br.target_pc}%x")
      if (br.target_pc == victim_dest_pc) {
        print(" (VICTIM DEST)\n")
      } else {
        print(" (POISONED DEST)\n")
      }
    }
  }


}

@main def testVCDPhantom() = {
  //applyXiangShan2ndGenParam()
  applyXiangShan2ndGenParam()
  test_vcd_phantom_body()
}
/*
@main def nativeTestVCD = {
  NativeTest.run(Array("1"), "testVCD")
}*/
/*
@main def testNative_XS = {
  applyXiangShan2ndGenParam()
  test52body
}*/