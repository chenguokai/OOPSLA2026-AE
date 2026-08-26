import HT.ASTUtils.printAST
import HT.{SMTCode, codeGen, printSMT}
import HT.CodeGen.*
import HT.StdLib.{Cacheline2Var, DCachePtrFlush, MainRet, PtrLoad, Var2Ptr}
import HT.Types.{Cacheline, SInt, UInt, UInt64, types}
import HT.*
import HT.ASTNodes.given_Conversion_ObjectValueNode_ArithNode
import HT.ASTNodes.given_Conversion_ValueNode_ArithNode
import HT.CachelineUtils.EvictSetFromL1DCacheline
import HT.Executors.pyStr2Result
import HT.MarchParameters
import HT.Permissions.Permission.{AttackerPrivate, VictimPublic}
import HT.given_Conversion_ValueNode_PlacementOperator
import HT.observation.*
import HT.simrunners.{XSRun, emu_path}
import HT.given_Conversion_Long_PlacementOperator
import HT.observation.AbstractionLayer.*

def test56body = {
  val attack: Boolean = true
  //val attack: Boolean = false

  val ast = Victim {
    Attacker() {
      val attack_dst = UInt64(0)
    }
    val victim_var = UInt64(0)
    val base_var = UInt64(0)

    Attacker() {
      val victim_ptr = Var2Ptr(victim_var)
      val base_ptr = Var2Ptr(base_var)
    }

    val main = Func(SInt)() {
      val i = UInt64(0)
      Control(){
        val total = UInt64(0)
      }

      While(i < imm(0x10000)) {
        Attacker() {
          if (!attack && (MarchParameters.ISA == "riscv64")) {
            DCachePtrFlush(refv("base_ptr"))
          } else {
            DCachePtrFlush(refv("victim_ptr"))
          }
        }
        if (attack)
          victim_var := imm(0)

        Control(){
          val cost = UInt64(0)
          Mfence()
          Timing(cost) {
            Attacker() {
              PtrLoad(refv("victim_ptr"), refv("attack_dst"))
              Mfence()
            }
          }
          refv("total") := refv("total") + cost
        }
        i := i + imm(1)
      }
      Control (){
        printInt(refv("total"))
      }
      MainRet(0)
    }
    Constrain(){
      AppendConstraint(victim_var.obj.saddr === 0x40000000L) // make it reproducible
      AppendConstraint(base_var.obj.saddr === 0x40000000L + MarchParameters.L1DLine)
    }
  }
  GlobalPass(ast)

  if (MarchParameters.ISA == "riscv64") {
    // do hardware test
    val vcd_path = XSRun(20000L)
    val LoadAddr = observation.getPC(refo("victim_var"))

    val parser = VCDParser(vcd_path.toString, XSDCacheSignalSet)
    println(s"VCD path: $vcd_path, max time: ${parser.getMaxTime}")

    val dcacheObj = new XSDCache(parser)

    var totalMiss = 0

    val reqMissInDCache = () => {
      dcacheObj.missReqFire(GlobalCycle) && dcacheObj.missReqMatchVaddr(GlobalCycle, LoadAddr)
    }

    setGraphRange(0, 41000)

    On(reqMissInDCache, "miss in DCache") {
      entry {
        totalMiss += 1
      }
    }

    println(s"Total miss in DCache: $totalMiss")
    generateGraph()
  }
}

@main def TestFlushReload_IA = {
  applyIntel14thGenParam() // apply to other platforms as well: amd zen4 and intel 7th gen
  test56body
}

@main def TestFlushReload_XS = {
  applyXiangShan2ndGenParam()
  test56body
}