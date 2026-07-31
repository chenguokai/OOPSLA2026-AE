import HT.ASTNodes.*
import HT.ASTUtils.printAST
import HT.CachelineUtils.EvictSetFromL1DCacheline
import HT.CodeGen.*
import HT.Executors.pyStr2Result
import HT.Permissions.Permission.VictimPublic
import HT.StdLib.*
import HT.Types.*
import HT.observation.*
import HT.observation.AbstractionLayer.*
import HT.simrunners.XSRun
import HT.*
import HT.ASTNodes.given_Conversion_ValueNode_ArithNode
import HT.ASTNodes.given_Conversion_Int_ValueNode
import HT.ASTNodes.given_Conversion_Long_ValueNode
import HT.ASTNodes.given_Conversion_Int_ArithNode
import HT.given_Conversion_Long_PlacementOperator


def test109body = {
  val ELFPath = "/home/xim-intel14/testhello.elf"
  val vcd_path = XSRun(100000, customELF = Some(ELFPath))
  ELFParseInit(ELFPath)
  val helloFunc = getELFAddr("helloFunc").head
  val parser = VCDParser(vcd_path.toString, XSIbufferSignalSet)

  val ibufObj = new XSIBuffer(parser)

  val blockInIbufStart = () => {
    ibufObj.PCWithinValid(GlobalCycle, helloFunc) >= 0 && ibufObj.outCanAccept(GlobalCycle)
  }

  On(blockInIbufStart, "Hello Function in Execution") {
    entry{
      EventLog(s"Hello Function in Ibuffer at Cycle $GlobalCycle")
    }
  }
}

@main def Test109_customELF(): Unit = {
  applyXiangShan2ndGenParam()
  test109body
}