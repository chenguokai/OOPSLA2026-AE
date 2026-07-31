import HT.ASTNodes.*
import HT.ASTUtils.printAST
import HT.CachelineUtils.EvictSetFromL1DCacheline
import HT.CodeGen.*
import HT.Executors.pyStr2Result
import HT.Permissions.Permission.{AttackerPublic, VictimPublic}
import HT.StdLib.*
import HT.Types.*
import HT.*
import HT.given_Conversion_Int_PlacementOperator
import HT.ASTNodes.given_Conversion_Int_ValueNode
import HT.ASTNodes.given_Conversion_Int_ArithNode

def test998body = {

  val ast = Victim {

    val jmp1 = ControlflowInst()
    val jmp2 = ControlflowInst()

    val main = Func(SInt)() {

      val cond = Bool(true)
      val A = UInt(1)
      val B = UInt(2)
      val counter = UInt(0)

      If((cond & 1) === 1, jmp1) {
        counter := counter + 1
      } .Else {
        counter := counter + 1
      }
      
      If ((cond & 1) === 0, jmp2) {
        counter := counter + 2
      }

      MainRet(0)
    }
    
    Constrain() {
      AppendConstraint(jmp1.saddr === 0x41000000)
      AppendConstraint(jmp2.saddr === 0x41001000)
    }

  }
  GlobalPass(ast)
}

@main def TestFar_IA = {
  applyIntel7thGenParam()
  test998body
}

@main def TestFar_XS = {
  applyXiangShan2ndGenParam()
  test998body
}