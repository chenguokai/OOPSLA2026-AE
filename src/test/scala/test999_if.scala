import HT.ASTUtils.printAST
import HT.{AppendConstraint, Attacker, Constrain, Control, ExactPadding, Func, GlobalPass, GlobalSolve, If, Intel14GInstrEvictionSet, Intel14GJmpCollisionPhantom, Jmp, Label, MarchParameters, Padding, PlaceLabel, SMTCode, Timing, Victim, While, XiangShan2ndGenParam, a, applyAMDZen4Param, applyIntel14thGenParam, applyIntel7thGenParam, applyXiangShan2ndGenParam, call, codeGen, given_Conversion_Int_PlacementOperator, given_Conversion_Long_PlacementOperator, given_Conversion_ValueNode_PlacementOperator, imm, outputGen, placement, printInt, printSMT, refo, refv, ret, tryRun}
import HT.CodeGen.*
import HT.StdLib.{Cacheline2Var, FlushBPHistory, MainRet, SyscallSwitch, USleepSwitch}
import HT.Types.{Bool, Cacheline, ControlflowInst, Inst, SInt, UInt, UInt64, types}
import HT.ASTNodes.given_Conversion_ObjectValueNode_ArithNode
import HT.ASTNodes.given_Conversion_ValueNode_ArithNode
import HT.CachelineUtils.EvictSetFromL1DCacheline
import HT.Executors.pyStr2Result
import HT.Permissions.Permission.{AttackerPublic, VictimPublic}
import HT.ASTNodes.given_Conversion_Int_ValueNode
import HT.ASTNodes.given_Conversion_Int_ArithNode
import HT.ASTNodes.given_Conversion_Long_ValueNode
import HT.StdLib.ICacheProbe

def test999body = {

  val ast = Victim {

    val main = Func(SInt)() {

      val cond = Bool(true)
      val A = UInt(1)
      val B = UInt(2)
      val counter = UInt(0)

      If((cond & 1) === 1) {
        counter := counter + 1
      } .Else {
        counter := counter + 1
      }

      MainRet(0)
    }

  }
  GlobalPass(ast)
}

@main def TestIf_IA = {
  applyIntel7thGenParam()
  test999body
}

@main def TestIf_XS = {
  applyXiangShan2ndGenParam()
  test999body
}