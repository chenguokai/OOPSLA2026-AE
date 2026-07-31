package HT

import HT.Types.types
import HT.ASTNodes.{ObjectValueNode, given_Conversion_ValueNode_ArithNode}
import HT.ASTNodes.given_Conversion_ObjectValueNode_ArithNode
import HT.ASTNodes.given_Conversion_Int_ArithNode

// march parameters of zen 4

object MarchParameters {
  var MarchName = "Illegal"
  var IsFPGA = false
  var ISA = "Unknown"
  var L1DSet = 64
  var L1DWay = 8
  var L1DLine = 64
  // following parameters may not be correct for my intel 14th Gen
  // but should be sufficient for testing
  var DefaultCore = 0
  var SMTCore = 1
  var DiffCore = 4
  var CrossPrefix: String = ""
  var compileTarget = ""

  var PaddingMaxGap = 10
  var NopSize = 1
  var OoOWindowSize = 128 // just a random value
  var BPHistorySize = 30

  var PageSize = 4096

  var L1IWay = 12 // Uop cache is wider than L1I cache
  var L1ISet = 64
  var L1ILine = 64

  // more params should be pushed onto the stack
  var ParamRegs = List("rdi", "rsi", "rdx", "rcx", "r8", "r9")
  var CallerReservedRegs = List("r10", "r11")
  var MaxParamReg = ParamRegs.size
}

object Intel14thGenParam {
  val MarchName = "Intel14thGen"
  val ISA = "x86_64"
  val L1DSet = 64
  val L1DWay = 8
  val L1DLine = 64
  // following parameters may not be correct for my intel 14th Gen
  // but should be sufficient for testing
  val DefaultCore = 0
  val SMTCore = 1
  val DiffCore = 4
  val CrossPrefix: String = ""
  val compileTarget = " --target=x86_64-linux-gnu -mcmodel=large "

  val PaddingMaxGap = 20
  val NopSize = 1
  val BPHistorySize = 300

  val PageSize = 4096

  val L1IWay = 8
  val L1ISet = 64
  val L1ILine = 64

  // more params should be pushed onto the stack
  val ParamRegs = List("rdi", "rsi", "rdx", "rcx", "r8", "r9")
  val CallerReservedRegs = List("r10", "r11")
  val MaxParamReg = ParamRegs.size
}

object Intel7thGenParam {
  val MarchName = "Intel7thGen"
  val ISA = "x86_64"
  val L1DSet = 64
  val L1DWay = 8
  val L1DLine = 64
  // following parameters may not be correct for my intel 14th Gen
  // but should be sufficient for testing
  val DefaultCore = 0
  val SMTCore = 1
  val DiffCore = 3
  val CrossPrefix: String = ""
  val compileTarget = " --target=x86_64-linux-gnu -mcmodel=large "

  val PaddingMaxGap = 20
  val NopSize = 1
  val BPHistorySize = 32 // 10 for BTB hit

  val PageSize = 4096

  val L1IWay = 8
  val L1ISet = 64
  val L1ILine = 64

  // more params should be pushed onto the stack
  val ParamRegs = List("rdi", "rsi", "rdx", "rcx", "r8", "r9")
  val CallerReservedRegs = List("r10", "r11")
  val MaxParamReg = ParamRegs.size
}


object AMDZen4Param {
  val MarchName = "AMDZen4"
  val ISA = "x86_64"
  val L1DSet = 64
  val L1DWay = 8
  val L1DLine = 64
  // following parameters may not be correct for my intel 14th Gen
  // but should be sufficient for testing
  val DefaultCore = 0
  val SMTCore = 16
  val DiffCore = 4
  val CrossPrefix: String = ""
  val compileTarget = " --target=x86_64-linux-gnu -mcmodel=large "

  val PaddingMaxGap = 20
  val NopSize = 1
  val BPHistorySize = 32768

  val PageSize = 4096

  val L1IWay = 12 // Uop cache is wider than L1I cache
  val L1ISet = 64
  val L1ILine = 64

  // more params should be pushed onto the stack
  val ParamRegs = List("rdi", "rsi", "rdx", "rcx", "r8", "r9")
  val CallerReservedRegs = List("r10", "r11")
  val MaxParamReg = ParamRegs.size
}


object XiangShan2ndGenParam {
  val MarchName = "XiangShanNanhu"
  val ISA = "riscv64"
  val L1DSet = 256
  val L1DWay = 8
  val L1DLine = 64
  val DefaultCore = 0
  val SMTCore = -1
  val DiffCore = 1
  // val CrossPrefix: String = "riscv64-linux-gnu-"
  val CrossPrefix: String = ""
  val compileTarget = " --target=riscv64-linux-gnu -march=rv64gczicbom_zicond -mabi=lp64d -mno-relax "

  val PaddingMaxGap = 20
  val NopSize = 2
  val BPHistorySize = 260

  val PageSize = 4096

  val L1IWay = 4  // Minimal Config
  val L1ISet = 64 // Minimal Config
  val L1ILine = 64

  val FTBSize = 256
  val FTBWays = 2
  val FTBSet = FTBSize / FTBWays
  val FTBTagSize = 20


  // more params should be pushed onto the stack
  val ParamRegs = List("a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7")
  val CallerReservedRegs = List("ra", "t0", "t1", "t2", "t3", "t4", "t5", "t6")
  val MaxParamReg = ParamRegs.size

  lazy val XiangShan2ndGenL1DCacheCollision = placement("XiangShan2ndL1DCacheCollision")("orig" -> types.Addr, "new" -> types.Addr) {
    a("new") > imm(0x10000000)
    a("new") % imm(L1DLine) === imm(0)
    a("orig") < a("new")
    (a("new") - a("orig")) % imm(L1DLine * L1DSet) === imm(0)
  }

  def L1ICacheConflict(orig: ObjectValueNode, next: ObjectValueNode) = {
    Constrain() {
      (orig.saddr + imm(L1ILine * L1ISet)) === next.saddr
    }
  }
}

def applyFPGAParam() = {
  MarchParameters.IsFPGA = true
}

def applyXiangShan2ndGenParam() = {
  MarchParameters.MarchName = XiangShan2ndGenParam.MarchName
  MarchParameters.ISA = XiangShan2ndGenParam.ISA
  MarchParameters.L1DSet = XiangShan2ndGenParam.L1DSet
  MarchParameters.L1DWay = XiangShan2ndGenParam.L1DWay
  MarchParameters.L1DLine = XiangShan2ndGenParam.L1DLine
  MarchParameters.DefaultCore = XiangShan2ndGenParam.DefaultCore
  MarchParameters.SMTCore = XiangShan2ndGenParam.SMTCore
  MarchParameters.DiffCore = XiangShan2ndGenParam.DiffCore
  MarchParameters.CrossPrefix = XiangShan2ndGenParam.CrossPrefix
  MarchParameters.compileTarget = XiangShan2ndGenParam.compileTarget

  MarchParameters.PaddingMaxGap = XiangShan2ndGenParam.PaddingMaxGap
  MarchParameters.NopSize = XiangShan2ndGenParam.NopSize
  MarchParameters.BPHistorySize = XiangShan2ndGenParam.BPHistorySize
  MarchParameters.ParamRegs = XiangShan2ndGenParam.ParamRegs
  MarchParameters.CallerReservedRegs = XiangShan2ndGenParam.CallerReservedRegs
  MarchParameters.MaxParamReg = XiangShan2ndGenParam.MaxParamReg

  MarchParameters.L1IWay = XiangShan2ndGenParam.L1IWay
  MarchParameters.L1ISet = XiangShan2ndGenParam.L1ISet
  MarchParameters.L1ILine = XiangShan2ndGenParam.L1ILine

}

def applyIntel14thGenParam() = {
  MarchParameters.MarchName = Intel14thGenParam.MarchName
  MarchParameters.ISA = Intel14thGenParam.ISA
  MarchParameters.L1DSet = Intel14thGenParam.L1DSet
  MarchParameters.L1DWay = Intel14thGenParam.L1DWay
  MarchParameters.L1DLine = Intel14thGenParam.L1DLine
  MarchParameters.DefaultCore = Intel14thGenParam.DefaultCore
  MarchParameters.SMTCore = Intel14thGenParam.SMTCore
  MarchParameters.DiffCore = Intel14thGenParam.DiffCore
  MarchParameters.CrossPrefix = Intel14thGenParam.CrossPrefix
  MarchParameters.compileTarget = Intel14thGenParam.compileTarget

  MarchParameters.PaddingMaxGap = Intel14thGenParam.PaddingMaxGap
  MarchParameters.NopSize = Intel14thGenParam.NopSize
  MarchParameters.BPHistorySize = Intel14thGenParam.BPHistorySize
  MarchParameters.ParamRegs = Intel14thGenParam.ParamRegs
  MarchParameters.CallerReservedRegs = Intel14thGenParam.CallerReservedRegs
  MarchParameters.MaxParamReg = Intel14thGenParam.MaxParamReg
}

def applyIntel7thGenParam() = {
  MarchParameters.MarchName = Intel7thGenParam.MarchName
  MarchParameters.ISA = Intel7thGenParam.ISA
  MarchParameters.L1DSet = Intel7thGenParam.L1DSet
  MarchParameters.L1DWay = Intel7thGenParam.L1DWay
  MarchParameters.L1DLine = Intel7thGenParam.L1DLine
  MarchParameters.DefaultCore = Intel7thGenParam.DefaultCore
  MarchParameters.SMTCore = Intel7thGenParam.SMTCore
  MarchParameters.DiffCore = Intel7thGenParam.DiffCore
  MarchParameters.CrossPrefix = Intel7thGenParam.CrossPrefix
  MarchParameters.compileTarget = Intel7thGenParam.compileTarget

  MarchParameters.PaddingMaxGap = Intel7thGenParam.PaddingMaxGap
  MarchParameters.NopSize = Intel7thGenParam.NopSize
  MarchParameters.BPHistorySize = Intel7thGenParam.BPHistorySize
  MarchParameters.ParamRegs = Intel7thGenParam.ParamRegs
  MarchParameters.CallerReservedRegs = Intel7thGenParam.CallerReservedRegs
  MarchParameters.MaxParamReg = Intel7thGenParam.MaxParamReg
}

def applyAMDZen4Param() = {
  MarchParameters.MarchName = AMDZen4Param.MarchName
  MarchParameters.ISA = AMDZen4Param.ISA
  MarchParameters.L1DSet = AMDZen4Param.L1DSet
  MarchParameters.L1DWay = AMDZen4Param.L1DWay
  MarchParameters.L1DLine = AMDZen4Param.L1DLine
  MarchParameters.DefaultCore = AMDZen4Param.DefaultCore
  MarchParameters.SMTCore = AMDZen4Param.SMTCore
  MarchParameters.DiffCore = AMDZen4Param.DiffCore
  MarchParameters.CrossPrefix = AMDZen4Param.CrossPrefix
  MarchParameters.compileTarget = AMDZen4Param.compileTarget

  MarchParameters.PaddingMaxGap = AMDZen4Param.PaddingMaxGap
  MarchParameters.NopSize = AMDZen4Param.NopSize
  MarchParameters.BPHistorySize = AMDZen4Param.BPHistorySize
  MarchParameters.ParamRegs = AMDZen4Param.ParamRegs
  MarchParameters.CallerReservedRegs = AMDZen4Param.CallerReservedRegs
  MarchParameters.MaxParamReg = AMDZen4Param.MaxParamReg
}

def Intel14GJmpCollisionPhantom(victim: ObjectValueNode, attacker: ObjectValueNode) = {
  AppendConstraint((attacker.saddr - victim.saddr)
    % imm(0x100000000L) === imm(8))
  AppendConstraint(attacker.saddr =/= victim.saddr)
  AppendConstraint(attacker.saddr <= imm(0x400000000L))
  AppendConstraint(attacker.saddr > imm(0x300000000L))
}

def Intel14GInstrEvictionSet(target: ObjectValueNode, eviction: List[ObjectValueNode], offset: Int) = {
  AppendConstraint(eviction.head.saddr === target.saddr + offset + MarchParameters.L1ISet * MarchParameters.L1ILine)
  (1 until eviction.size).map {
    i => AppendConstraint(eviction(i).saddr === eviction(i - 1).saddr + MarchParameters.L1ILine * MarchParameters.L1ISet)
  }
}

def AMD64CondBrCollision(victim: ObjectValueNode, attacker: ObjectValueNode) = {
  AppendConstraint(attacker.saddr - victim.saddr > 0)
  AppendConstraint((attacker.saddr - victim.saddr) % 0x400000000000L === 0)
  AppendConstraint(attacker.saddr < 0x800000000000L)
}

def XiangShanJmpCollisionPhantom(victim: ObjectValueNode, attacker: ObjectValueNode) = {
  AppendConstraint((attacker.saddr - victim.saddr) % imm(0x20000000) === imm(0))
  AppendConstraint(attacker.saddr < imm(0x80000000))
  AppendConstraint(attacker.saddr > imm(0))
}

def XiangShanCondBrCollision(victim: ObjectValueNode, attacker: ObjectValueNode) = {
  AppendConstraint(victim.saddr - attacker.saddr > 0)
  AppendConstraint((victim.saddr - attacker.saddr) % 0x40000000 === 0)
  AppendConstraint(attacker.saddr > 0x30000000)
}

def Unique(a: List[ObjectValueNode]) = {
  // ensure that every element has a different address
  for (i <- a.indices) {
    for (j <- i + 1 until a.size) {
      AppendConstraint(a(i).saddr =/= a(j).saddr)
    }
  }
}

def UniqueLines(a: PlacementOperator, b: PlacementOperator) = {
  // ensure that every element has a different cache line
  (a / MarchParameters.L1DLine) =/= (b / MarchParameters.L1DLine)
}

def SetIndexOld(addr: PlacementOperator) = {
  // old L1D cache index function
  (addr / MarchParameters.L1DLine) % MarchParameters.L1DSet
}

def SetIndexNew(addr: PlacementOperator) = {
  // new L1D cache index function
  BitFold(addr / 4096, width = 2, length = 36)
}

def FarAway(a: ObjectValueNode, b: ObjectValueNode) = {
  AppendConstraint(a.dcacheline === b.dcacheline - 100)
}

def OoOWindow(former: ObjectValueNode, latter: ObjectValueNode) = {
  // ensure that the two instructions are within a single OoO window
  // actually stricter than necessary, since for x86/rvc
  // a precise window size is only available with asm level information
  AppendConstraint(latter.saddr - former.saddr < MarchParameters.NopSize * MarchParameters.OoOWindowSize)
}

def NextDLine(first: ObjectValueNode, second: ObjectValueNode) = {
  AppendConstraint(first.dcacheline + 1 === second.dcacheline)
}

def EvictionSet(target: ObjectValueNode, lines: List[ObjectValueNode]) = {
  if (lines.size != MarchParameters.L1DWay) {
    throw new Exception("Mismatch between eviction set size and March Param")
  }
  AppendConstraint(lines.head.saddr > target.saddr)
  AppendConstraint((lines.head.saddr - target.saddr) % (MarchParameters.L1DLine * MarchParameters.L1DSet) === 0)
  for (i <- 1 until lines.size) {
    AppendConstraint(lines(i).saddr === lines(i - 1).saddr + MarchParameters.L1DLine * MarchParameters.L1DSet)
  }
}