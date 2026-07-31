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

def test110body = {
  val ELFPath = "/home/xim-intel14/ELF/riscv-rootfs/luainit2.elf"
  //  "/mnt/ssd4t/home/xim-intel14/luahard.vcd" "/mnt/ssd4t/home/xim-intel14/luabase.vcd"
  val vcd_path = XSRun(1000000, customELF = Some(ELFPath))
  val PythonPath = "/home/xim-intel14/dwarf/lua/lua"
  ELFParseInit(PythonPath)
  val parser = VCDParser(vcd_path.toString, XSBPUSignalSet)

  val bpuObj = new XSBPU(parser)

  var mispredMap = Map[BigInt, Int]()

  val mispredCond = () => {
    bpuObj.hasMispred(GlobalCycle)
  }

  On(mispredCond, "mispredictions") {
    entry{
      val pc = bpuObj.MispredPC(GlobalCycle)
      mispredMap += (pc -> (mispredMap.getOrElse(pc, 0) + 1))
    }
  }

  // println(s"Misprediction Map: ${mispredMap}")
  // for each (pc, count) in mispredMap print
  println("Misprediction Map:")
  mispredMap.toList.sortBy(-_._2).foreach { case (pc, count) =>
    println(f"PC: 0x${pc.toLong}%016x, Count: $count")
  }

}

@main def TestPythonInterpreter_XS = {
  applyXiangShan2ndGenParam()
  test110body
}