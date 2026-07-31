import HT.ASTUtils.printAST
import HT.{SMTCode, codeGen, printSMT}
import HT.CodeGen.*
import HT.StdLib.{Cacheline2Var, MainRet}
import HT.Types.{Cacheline, ControlflowInst, MakeNVars, SInt, UInt, UInt64, types}
import HT.*
import HT.ASTNodes.given_Conversion_ObjectValueNode_ArithNode
import HT.ASTNodes.given_Conversion_ValueNode_ArithNode
import HT.CachelineUtils.EvictSetFromL1DCacheline
import HT.Executors.pyStr2Result
import HT.MarchParameters
import HT.ASTNodes.given_Conversion_Int_ArithNode
import HT.Permissions.Permission.{AttackerPrivate, VictimPublic}
import HT.given_Conversion_Long_PlacementOperator
import HT.ASTNodes.given_Conversion_Int_ValueNode
import HT.given_Conversion_Int_PlacementOperator
import HT.given_Conversion_ValueNode_PlacementOperator
import HT.observation.*
import HT.simrunners.{XSRun, emu_path}
import HT.observation.AbstractionLayer.*

def test21_body = {
  val attack = true
  val ast = Victim{
    // val controlInst = ControlflowInst() // for testing unused obj

    val line = UInt64(0)

    Attacker() {
      val vars = MakeNVars(MarchParameters.L1DWay, UInt64(0))
    }

    Control(){
      val total = UInt64(0)
      val tmp = UInt64(0)
    }
    
    val vars1 = MakeNVars(MarchParameters.L1DWay, UInt64(0))

    Constrain() {
      val alist = (0 until MarchParameters.L1DWay).map {
        i => refo(s"vars_$i")
      }.toList
      val vlist = (0 until MarchParameters.L1DWay).map {
        i => vars1(i).obj
      }.toList
      refo("vars_0").saddr === imm(0x40000000L) // make it reproducible
      EvictionSet(line.obj, alist)
      EvictionSet(line.obj, vlist)
      AppendConstraint(line.obj.saddr > 0x10000000L)
      AppendConstraint(line.obj.saddr < 0x80000000L)
      Unique(alist ++ vlist)
      // val expr = controlInst.saddr === 0x8000000 // for testing unused obj
    }

    val main = Func(SInt)() {
      val i = UInt(0)
      While(i < (if (MarchParameters.ISA == "riscv64") 50 else 1000000)) {
        Control(){
          Attacker() {
            (0 until MarchParameters.L1DWay).map {
              l => refv(s"vars_$l") := 1
            }
          }
          val cost = UInt(0)
        }

        if (attack) {
          (0 until MarchParameters.L1DWay).map {
            l => vars1(l) := 0
          }
        }


        Control(){
          Timing(refv("cost")) {
            Attacker() {
              val gtmp = refv("tmp")
              (0 until MarchParameters.L1DWay).map {
                i => {
                  Load(refv(s"vars_$i"), gtmp)
                  Mfence()
                }
              }
            }
          }
          If(refv("cost") < (if (MarchParameters.ISA == "riscv64") 300 else 2000)) {
            refv("total") := refv("total") + refv("cost") // total + cost
          }
        }

        i := i + imm(1)
      }
      Control(){
        printInt(refv("total"))
      }
      MainRet(0)
    }
  }
  GlobalPass(ast)

  if (MarchParameters.ISA == "riscv64") {
    // do hardware test
    val vcd_path = XSRun(100000L) // /mnt/ssd4t/home/xim-intel14/primeprobe.vcd

    val storeAddrs = (0 until MarchParameters.L1DWay).map { i =>
      observation.getPC(refo(s"vars_$i"))
    }.toList

    val parser = VCDParser(vcd_path.toString, XSDCacheSignalSet)
    println(s"VCD path: $vcd_path, max time: ${parser.getMaxTime}")

    val dcacheObj = new XSDCache(parser)

    var totalEviction = 0
    var vaddr: BigInt = 0
    var totalMiss = 0

    val reqInDCache = () => {
      storeAddrs.map { addr =>
        dcacheObj.reqMatchVaddr(GlobalCycle, addr)
      }.reduce(_ || _)
    }
    val reqMissInDCache = () => {
      dcacheObj.missReqFire(GlobalCycle) && storeAddrs.map { addr =>
         dcacheObj.missReqMatchVaddr(GlobalCycle, addr)
      }.reduce(_ || _)
    }

    setGraphRange(28000, 68000)

    On(reqInDCache, "store request in DCache") {
      entry{
        if (dcacheObj.reqNeedsEviction(GlobalCycle)) {
          vaddr = dcacheObj.reqVaddr(GlobalCycle)
          totalEviction += 1
        }
        EventLog(f"Store request in DCache for vaddr: ${vaddr}%X")
      }
    }

    GlobalCycle = 0 // reset cycle count for next till

    On(reqMissInDCache, "store request miss in DCache") {
      entry{
        totalMiss += 1
      }
    }

    println(s"Total eviction in DCache: $totalEviction")
    println(s"Total miss in DCache: $totalMiss")
    generateGraph()
  }
}

@main def TestPrimeProbeDSL_IA = {
  applyIntel14thGenParam() // also works for Intel7thGen
  test21_body
}

@main def TestPrimeProbeDSL_AMD = {
  applyAMDZen4Param()
  test21_body
}

@main def TestPrimeProbeDSL_XS = {
  applyXiangShan2ndGenParam()
  test21_body
}