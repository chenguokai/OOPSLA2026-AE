package HT.ASTNodes

import HT.Permissions.Permission.VictimPrivate
import HT.{ASTNodes, AttackerZones, ControlAttackerInterestingLabels, FuncAddrConstraintStack, FuncParameter, GlobalObjectStack, GlobalStack, InterestingLabels, PaddingStack, PlacementEntityStack, PlacementOperator, PlacementParameter, refo}
import HT.Types.{ArrayElementNode, AsmBlock, Bool, Cacheline, ControlflowInst, ExactPaddingNode, Int16, Int32, Int64, Int8, LabelNode, LoadInst, PaddingNode, SInt, StoreInst, TLBEntry, TLBPermission, UInt, UInt16, UInt32, UInt64, UInt8, ValueBaseTypes, ValueTypes, types}
import HT.Permissions.{CurrentDefaultPermission, InferPermission, PermCompatible, PermEqual, Permission, WorldPermCompatible}

import scala.annotation.targetName
import scala.collection.mutable.{Map, Set}

val SATParamSet: Set[String] = Set() // store the uniname of object/function

val AlignMap: Map[String, Int] = Map() // key: uniname, value: align
val BaseConstrainSet: Set[String] = Set() // key: uniname, value: base align

sealed trait ASTNode
sealed trait ValueBaseNode extends ASTNode
case class operatorNode(toperator: String) extends ASTNode
case class WhenNode(uniname: String, condition: ConditionNode, body: List[ASTNode], branch: Option[ObjectValueNode], elseWhens: List[ElseWhenNode] = List(), otherwiseBody: OtherwiseNode = OtherwiseNode(List()), padding: Option[PaddingNode] = None) extends ASTNode
case class ElseWhenNode(condition: ConditionNode, body: List[ASTNode], branch: Option[ObjectValueNode], padding: Option[PaddingNode]) extends ASTNode
case class OtherwiseNode(body: List[ASTNode]) extends ASTNode
case class StatementNode(statement: String) extends ASTNode
case class ConditionNode(toperator: operatorNode, body: List[ASTNode], perm: Permission) extends ASTNode
case class TimingNode(dst: ValueNode, name: String, body: List[ASTNode]) extends ASTNode
case class AssignNode(left: ValueNode, right: ArithNode, perm: Permission) extends ASTNode

case class ArrayNode(nm: String, decl: DeclareNode) extends ASTNode {
  def at(idx: ArithNode): ValueNode = {
    GlobalStack.pop(1) // pop ArithNode
    val body = ArrayElementNode(nm, idx)
    val declNode = DeclareNode.apply_no_push(CurrentDefaultPermission, types.ArrayElement, body, nm)
    val valueNode = ValueNode(declNode, nm)
    valueNode
  }
}
case class ArithNode(toperator: operatorNode, body: List[ASTNode], perm: Permission) extends ASTNode {
  def <(that: ArithNode): ConditionNode = {
    val conditionStr = "<"
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.perm, that.perm)
    val ret = ConditionNode(operator, List(this, that), perm)
    GlobalStack.pop(2) // pop two ArithNode
    GlobalStack.push(ret)
    ret
  }
  def >(that: ArithNode): ConditionNode = {
    val conditionStr = ">"
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.perm, that.perm)
    val ret = ConditionNode(operator, List(this, that), perm)
    GlobalStack.pop(2) // pop two ArithNode
    GlobalStack.push(ret)
    ret
  }
  def ===(that: ArithNode): ConditionNode = {
    val conditionStr = "=="
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.perm, that.perm)
    val ret = ConditionNode(operator, List(this, that), perm)
    GlobalStack.pop(2) // pop two ArithNode
    GlobalStack.push(ret)
    ret
  }

  def =/=(that: ArithNode): ConditionNode = {
    val conditionStr = "!="
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.perm, that.perm)
    val ret = ConditionNode(operator, List(this, that), perm)
    GlobalStack.pop(2) // pop two ArithNode
    GlobalStack.push(ret)
    ret
  }

  def &&(that: ArithNode): ConditionNode = {
    val conditionStr = "&&"
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.perm, that.perm)
    val ret = ConditionNode(operator, List(this, that), perm)
    GlobalStack.pop(2) // pop two ArithNode
    GlobalStack.push(ret)
    ret
  }

  def ||(that: ArithNode): ConditionNode = {
    val conditionStr = "||"
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.perm, that.perm)
    val ret = ConditionNode(operator, List(this, that), perm)
    GlobalStack.pop(2) // pop two ArithNode
    GlobalStack.push(ret)
    ret
  }

  def ^(that: ArithNode): ArithNode = {
    val conditionStr = "^"
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.perm, that.perm)
    GlobalStack.pop(2) // pop two ArithNode
    val ret = ArithNode(operator, List(this, that), perm)
    GlobalStack.push(ret)
    ret
  }

  def +(that: ArithNode): ArithNode = {
    val conditionStr = "+"
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.perm, that.perm)
    GlobalStack.pop(2) // pop two ArithNode
    val ret = ArithNode(operator, List(this, that), perm)
    GlobalStack.push(ret)
    ret
  }
  def -(that: ArithNode): ArithNode = {
    val conditionStr = "-"
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.perm, that.perm)
    GlobalStack.pop(2) // pop two ArithNode
    val ret = ArithNode(operator, List(this, that), perm)
    ret
  }

  def &(that: ArithNode): ArithNode = {
    val conditionStr = "&"
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.perm, that.perm)
    GlobalStack.pop(2) // pop two ArithNode
    val ret = ArithNode(operator, List(this, that), perm)
    ret
  }

  def |(that: ArithNode): ArithNode = {
    val conditionStr = "|"
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.perm, that.perm)
    GlobalStack.pop(2) // pop two ArithNode
    val ret = ArithNode(operator, List(this, that), perm)
    ret
  }
  def *(that: ArithNode): ArithNode = {
    val conditionStr = "*"
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.perm, that.perm)
    GlobalStack.pop(2) // pop two ArithNode
    val ret = ArithNode(operator, List(this, that), perm)
    GlobalStack.push(ret)
    ret
  }
  def /(that: ArithNode): ArithNode = {
    val conditionStr = "/"
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.perm, that.perm)
    GlobalStack.pop(2) // pop two ArithNode
    val ret = ArithNode(operator, List(this, that), perm)
    GlobalStack.push(ret)
    ret
  }
  def %(that: ArithNode): ArithNode = {
    println("debug mod")
    val conditionStr = "%"
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.perm, that.perm)
    GlobalStack.pop(2) // pop two ArithNode
    val ret = ArithNode(operator, List(this, that), perm)
    GlobalStack.push(ret)
    ret
  }
  def <<(that: ArithNode): ArithNode = {
    val conditionStr = "<<"
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.perm, that.perm)
    GlobalStack.pop(2) // pop two ArithNode
    val ret = ArithNode(operator, List(this, that), perm)
    GlobalStack.push(ret)
    ret
  }
  def >>(that: ArithNode): ArithNode = {
    val conditionStr = ">>"
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.perm, that.perm)
    GlobalStack.pop(2) // pop two ArithNode
    val ret = ArithNode(operator, List(this, that), perm)
    GlobalStack.push(ret)
    ret
  }
}
case class DeclareNode(perm: Permission, typ: types, body: ValueBaseTypes, name: String, linkedObj: Option[ObjectValueNode] = None, rule: Option[PlacementNode] = None, applied: Boolean) extends ASTNode

case class ObjectDeclareNode(perm: Permission, typ: types, body: ValueBaseTypes, name: String, applied: Boolean) extends ASTNode

case class LoopNode(condition: ConditionNode, body: List[ASTNode], branch: Option[ObjectValueNode], padding: Option[PaddingNode] = None, uniname: String) extends ASTNode

case class BreakNode(branch: Option[ObjectValueNode], padding: Option[PaddingNode], uniname: String) extends ASTNode

case class TLBEntryPermissionChangeNode(src: PlacementOperator, perm: TLBPermission) extends ASTNode
case class TLBEntryUnmapNode(src: ValueNode) extends ASTNode

case class FlushNode(sets: List[ValueNode]) extends ASTNode
case class ProbeNode(sets: List[ValueNode]) extends ASTNode

case class ThreadJoinNode(pidVar: ValueNode) extends ASTNode
case class ProcessJoinNode(pidVar: ValueNode) extends ASTNode

case class FlushBPHistoryNode(inst: Option[ObjectValueNode], padding: Option[PaddingNode], uniname: String) extends ASTNode
case class YieldNode() extends ASTNode


case class SyscallSwitchNode() extends ASTNode
case class SleepSwitchNode(unit: Long) extends ASTNode
case class MainRetNode(code: Int) extends ASTNode
case class DCacheFlushNode(node: ValueNode, lineCount: Int, inst: Option[ObjectValueNode], padding: Option[PaddingNode], uniname: String) extends ASTNode
case class SequentialComputingDelayNode(node: ValueNode, rep: Int) extends ASTNode
case class FlushDCachePtrNode(ptr: ValueNode) extends ASTNode
case class PtrLoadNode(ptr: ValueNode, dst: ValueNode) extends ASTNode
case class Crc32ComputeNode(src: List[ValueNode], key: List[ValueNode]) extends ASTNode
case class IndirectCallNode(func: FunctionNode, branch: Option[ObjectValueNode], uniqueVar: String, args: List[ValueBaseNode], padding: Option[PaddingNode]) extends ASTNode
case class FlushICacheNode(inst: Option[ObjectValueNode], padding: Option[PaddingNode], uniname: String) extends ASTNode

case class AsmOperand(constrain: String, value: ValueNode)

case class InlineAsmNode(body: List[String], outputs: List[AsmOperand], inputs: List[AsmOperand], clobbers: List[String], handle: Option[ObjectValueNode]) extends ASTNode

case class FunctionDeclNode(name: String, parameters: List[FuncParameter], ret: types, body: List[ASTNode], rule: Option[PlacementNode], placementObjects: List[ObjectDeclareNode], perm: Permission) extends ASTNode
case class FunctionNode(decl: FunctionDeclNode, name: String) extends ASTNode {
  def apply(inst: Option[ObjectValueNode] = None, args: ValueBaseNode*): ValueBaseNode = {
    val padding = if (inst.isDefined) {
      // push inst onto stack
      FuncAddrConstraintStack.push(inst.get.decl)
      GlobalObjectStack.push(inst.get.decl)
      InterestingLabels.insert(inst.get.decl.body.asInstanceOf[ControlflowInst].uniname)
      if (PaddingStack.size() == 0) {
        None
      } else {
        val ret = PaddingStack.top()
        PaddingStack.clear()
        Some(ret)
      }
    } else {
      None
    }
    val ret = CallNode(this, args.toList, inst, padding)

    // push stack such that f() without return value receiver works
    GlobalStack.push(ret)

    ret
  }
  def obj: ObjectValueNode = {
    refo(this.name)
  }
}

case class ReturnNode(value: Option[ArithNode], branch: Option[ObjectValueNode], padding: Option[PaddingNode], uniname: String) extends ASTNode

case class CallNode(func: FunctionNode, args: List[ValueBaseNode], inst: Option[ObjectValueNode], padding: Option[PaddingNode]) extends ValueBaseNode
case class PlacementValueNode(node: PlacementNode)

case class PrintIntNode(value: ValueNode) extends ASTNode
case class PrintMultipleNode(form: String, param: List[ValueNode]) extends ASTNode

case class MFenceNode(inst: Option[ObjectValueNode], padding: Option[PaddingNode], uniname: String) extends ASTNode
case class PlacementDeclNode(name: String, parameters: List[PlacementParameter], body: List[ASTNode], applied: Boolean) extends ASTNode
case class PlacementNode(decl: PlacementDeclNode, name: String/*, addr: Long*/) extends ASTNode {
  def apply(): PlacementValueNode = {
    // should not accept any further arg from usage
    val ret = PlacementValueNode(this)

    // do not push stack because we should never use an object alone
    ret
  }
}

case class LoadNode(src: ValueNode, dst: ValueNode, inst: Option[ObjectValueNode], padding: Option[PaddingNode], uniname: String) extends ASTNode
case class StoreNode(src: ValueNode, dst: ValueNode, inst: Option[ObjectValueNode], padding: Option[PaddingNode], uniname: String, noderef: Boolean) extends ASTNode

case class WorldNode(typ: String, body: List[ASTNode], zone: Option[AttackerZones], pidVar: Option[ValueNode], applied: Boolean) extends ASTNode

case class PrimitiveNode(name: String, preamble: List[ASTNode], body: List[ASTNode], applied: Boolean) extends ASTNode

case class PullToLocalNode(pid: ValueNode, local: ValueNode, remote: ValueNode) extends ASTNode

case class ValueNode(decl: DeclareNode, name: String) extends ValueBaseNode {
  def obj: ObjectValueNode = {
    refo(this.name)
  }
  // define > operator for Bool
  def >(that: ValueNode): ConditionNode = {
    // if we are referred at a world without proper permission, throw exception
    WorldPermCompatible(this.decl.perm)
    WorldPermCompatible(that.decl.perm)
    val conditionStr = ">"
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.decl.perm, that.decl.perm)
    val ret = ConditionNode(operator, List(this, that), perm)
    //GlobalStack.pop(2) // pop two ValueNode
    GlobalStack.push(ret)
    ret
  }

  // define < operator for Bool
  def <(that: ValueNode): ConditionNode = {
    val conditionStr = "<"
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.decl.perm, that.decl.perm)
    val ret = ConditionNode(operator, List(this, that), perm)
    //GlobalStack.pop(2) // pop two ValueNode
    GlobalStack.push(ret)
    ret
  }

  def =/=(that: ValueNode): ConditionNode = {
    val conditionStr = "!="
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.decl.perm, that.decl.perm)
    val ret = ConditionNode(operator, List(this, that), perm)
    //GlobalStack.pop(2) // pop two ValueNode
    GlobalStack.push(ret)
    ret
  }

  def ===(that: ValueNode): ConditionNode = {
    val conditionStr = "=="
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.decl.perm, that.decl.perm)
    val ret = ConditionNode(operator, List(this, that), perm)
    //GlobalStack.pop(2) // pop two ValueNode
    GlobalStack.push(ret)
    ret
  }

  def &&(that: ValueNode): ConditionNode = {
    val conditionStr = "&&"
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.decl.perm, that.decl.perm)
    val ret = ConditionNode(operator, List(this, that), perm)
    //GlobalStack.pop(2) // pop two ValueNode
    GlobalStack.push(ret)
    ret
  }

  def ||(that: ValueNode): ConditionNode = {
    val conditionStr = "||"
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.decl.perm, that.decl.perm)
    val ret = ConditionNode(operator, List(this, that), perm)
    //GlobalStack.pop(2) // pop two ValueNode
    GlobalStack.push(ret)
    ret
  }

  def ^(that: ValueNode): ArithNode = {
    if (this.decl.typ == types.Bool || that.decl.typ == types.Bool) {
      throw new Exception("Type not supported for xor: " + this.decl.typ)
    }
    val conditionStr = "^"
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.decl.perm, that.decl.perm)
    val ret = ArithNode(operator, List(this, that), perm)
    //GlobalStack.pop(2) // pop two ValueNode
    GlobalStack.push(ret)
    ret
  }

  def +(that: ValueNode): ArithNode = {
    if (this.decl.typ == types.Bool || that.decl.typ == types.Bool) {
      throw new Exception("Type not supported for addition: " + this.decl.typ)
    }
    val conditionStr = "+"
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.decl.perm, that.decl.perm)
    val ret = ArithNode(operator, List(this, that), perm)
    //GlobalStack.pop(2) // pop two ValueNode
    GlobalStack.push(ret)
    ret
  }
  def -(that: ValueNode): ArithNode = {
    if (this.decl.typ == types.Bool || that.decl.typ == types.Bool) {
      throw new Exception("Type not supported for subtraction: " + this.decl.typ)
    }
    val conditionStr = "-"
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.decl.perm, that.decl.perm)
    val ret = ArithNode(operator, List(this, that), perm)
    //GlobalStack.pop(2) // pop two ValueNode
    GlobalStack.push(ret)
    ret
  }
  def *(that: ValueNode): ArithNode = {
    if (this.decl.typ == types.Bool || that.decl.typ == types.Bool) {
      throw new Exception("Type not supported for multiplication: " + this.decl.typ)
    }
    val conditionStr = "*"
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.decl.perm, that.decl.perm)
    val ret = ArithNode(operator, List(this, that), perm)
    //GlobalStack.pop(2) // pop two ValueNode
    GlobalStack.push(ret)
    ret
  }
  def /(that: ValueNode): ArithNode = {
    if (this.decl.typ == types.Bool || that.decl.typ == types.Bool) {
      throw new Exception("Type not supported for division: " + this.decl.typ)
    }
    val conditionStr = "/"
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.decl.perm, that.decl.perm)
    val ret = ArithNode(operator, List(this, that), perm)
    //GlobalStack.pop(2) // pop two ValueNode
    GlobalStack.push(ret)
    ret
  }
  def %(that: ValueNode): ArithNode = {
    if (this.decl.typ == types.Bool || that.decl.typ == types.Bool) {
      throw new Exception("Type not supported for modulo: " + this.decl.typ)
    }
    val conditionStr = "%"
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.decl.perm, that.decl.perm)
    val ret = ArithNode(operator, List(this, that), perm)
    //GlobalStack.pop(2) // pop two ValueNode
    GlobalStack.push(ret)
    ret
  }

  def &(that: ValueNode): ArithNode = {
    val conditionStr = "&"
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.decl.perm, that.decl.perm)
    val ret = ArithNode(operator, List(this, that), perm)
    //GlobalStack.pop(2) // pop two ValueNode
    GlobalStack.push(ret)
    ret
  }

  def |(that: ValueNode): ArithNode = {
    val conditionStr = "|"
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.decl.perm, that.decl.perm)
    val ret = ArithNode(operator, List(this, that), perm)
    //GlobalStack.pop(2) // pop two ValueNode
    GlobalStack.push(ret)
    ret
  }

  def <<(that: ValueNode): ArithNode = {
    if (this.decl.typ == types.Bool || that.decl.typ == types.Bool) {
      throw new Exception("Type not supported for left shift: " + this.decl.typ)
    }
    val conditionStr = "<<"
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.decl.perm, that.decl.perm)
    val ret = ArithNode(operator, List(this, that), perm)
    //GlobalStack.pop(2) // pop two ValueNode
    GlobalStack.push(ret)
    ret
  }

  def >>(that: ValueNode): ArithNode = {
    if (this.decl.typ == types.Bool || that.decl.typ == types.Bool) {
      throw new Exception("Type not supported for right shift: " + this.decl.typ)
    }
    val conditionStr = ">>"
    val operator = operatorNode(conditionStr)
    val perm = InferPermission(this.decl.perm, that.decl.perm)
    val ret = ArithNode(operator, List(this, that), perm)
    //GlobalStack.pop(2) // pop two ValueNode
    GlobalStack.push(ret)
    ret
  }

  def :=(that: ArithNode): AssignNode = {
    // if perm(that) is not equal to perm(this), throw exception
    // FIXME: reintroduce permission check
    PermCompatible(this.decl.perm, that.perm)
    val perm = this.decl.perm
    /*
    if (this.decl.typ != that..typ) {
      throw new Exception("Type mismatch for assignment: " + this.decl.typ + " and " + that.decl.typ)
    }*/
    val ret = AssignNode(this, that, perm)
    GlobalStack.pop(1) // pop ArithNode
    GlobalStack.push(ret)
    ret
  }
}

case class ObjectValueNode(decl: ObjectDeclareNode, name: String) extends ValueBaseNode {
  def saddr: PlacementOperator = {
    val uniname = this.decl.typ match {
      case types.BranchInst => {
        val nm = this.decl.body.asInstanceOf[ControlflowInst].uniname
        BaseConstrainSet.add(nm)
        nm
      }
      case types.LoadInst => {
        val nm = this.decl.body.asInstanceOf[LoadInst].uniname
        BaseConstrainSet.add(nm)
        nm
      }
      case types.StoreInst => {
        val nm = this.decl.body.asInstanceOf[StoreInst].uniname
        BaseConstrainSet.add(nm)
        nm
      }
      case types.ExactPadding => this.decl.body.asInstanceOf[ExactPaddingNode].name
      case types.Label => {
        val nm = this.decl.body.asInstanceOf[LabelNode].name
        BaseConstrainSet.add(nm)
        nm
      }
      case types.Func => {
        val nm = this.name
        BaseConstrainSet.add(nm)
        nm
      }
      case types.CacheLine => this.decl.body.asInstanceOf[Cacheline].uniname
      case types.TLBEntry => this.decl.body.asInstanceOf[TLBEntry].uniname
      case types.UInt64 => {
        val nm = this.decl.body.asInstanceOf[UInt64].uniname
        AlignMap.put(nm, 8)
        nm
      }
      case types.UInt32 => {
        val nm = this.decl.body.asInstanceOf[UInt32].uniname
        AlignMap.put(nm, 4)
        nm
      }
      case types.UInt16 => {
        val nm = this.decl.body.asInstanceOf[UInt16].uniname
        AlignMap.put(nm, 2)
        nm
      }
      case types.UInt8 => {
        val nm = this.decl.body.asInstanceOf[UInt8].uniname
        AlignMap.put(nm, 1)
        nm
      }
      case types.Bool => {
        val nm = this.decl.body.asInstanceOf[Bool].uniname
        AlignMap.put(nm, 1)
        nm
      }
      case types.UInt => {
        val nm = this.decl.body.asInstanceOf[UInt].uniname
        AlignMap.put(nm, 4)
        nm
      }
      case types.SInt => {
        val nm = this.decl.body.asInstanceOf[SInt].uniname
        AlignMap.put(nm, 4)
        nm
      }
      case types.Int64 => {
        val nm = this.decl.body.asInstanceOf[Int64].uniname
        AlignMap.put(nm, 8)
        nm
      }
      case types.Int32 => {
        val nm = this.decl.body.asInstanceOf[Int32].uniname
        AlignMap.put(nm, 4)
        nm
      }
      case types.Int16 => {
        val nm = this.decl.body.asInstanceOf[Int16].uniname
        AlignMap.put(nm, 2)
        nm
      }
      case types.Int8 => {
        val nm = this.decl.body.asInstanceOf[Int8].uniname
        AlignMap.put(nm, 1)
        nm
      }
      case types.AsmBlock => {
        val nm = this.decl.body.asInstanceOf[AsmBlock].uniname
        AlignMap.put(nm, 2)
        nm
      }
      case _ => throw new Exception("Unsupported object type" + this.decl.typ)
    }
    val ret = PlacementOperator("saddr", None, None, uniname)
    // PlacementEntityStack.push(ret)
    // add this node to parameter set of SAT solver
    SATParamSet.add(uniname)

    ret
  }

  def cacheline(t: String): PlacementOperator = {
    val uniname = this.decl.typ match {
      case types.BranchInst => {
        val nm = this.decl.body.asInstanceOf[ControlflowInst].uniname
        BaseConstrainSet.add(nm)
        nm
      }
      case types.LoadInst => {
        val nm = this.decl.body.asInstanceOf[LoadInst].uniname
        BaseConstrainSet.add(nm)
        nm
      }
      case types.StoreInst => {
        val nm = this.decl.body.asInstanceOf[StoreInst].uniname
        BaseConstrainSet.add(nm)
        nm
      }
      case types.ExactPadding => this.decl.body.asInstanceOf[ExactPaddingNode].name
      case types.Label => {
        val nm = this.decl.body.asInstanceOf[LabelNode].name
        BaseConstrainSet.add(nm)
        nm
      }
      case types.Func => {
        val nm = this.name
        BaseConstrainSet.add(nm)
        nm
      }
      case types.CacheLine => this.decl.body.asInstanceOf[Cacheline].uniname
      case types.TLBEntry => this.decl.body.asInstanceOf[TLBEntry].uniname
      case types.UInt64 => {
        val nm = this.decl.body.asInstanceOf[UInt64].uniname
        AlignMap.put(nm, 8)
        nm 
      }
      case types.UInt32 => {
        val nm = this.decl.body.asInstanceOf[UInt32].uniname
        AlignMap.put(nm, 4)
        nm
      }
      case types.UInt16 => {
        val nm = this.decl.body.asInstanceOf[UInt16].uniname
        AlignMap.put(nm, 2)
        nm
      }
      case types.UInt8 => {
        val nm = this.decl.body.asInstanceOf[UInt8].uniname
        AlignMap.put(nm, 1)
        nm
      }
      case types.Bool => {
        val nm = this.decl.body.asInstanceOf[Bool].uniname
        AlignMap.put(nm, 1)
        nm
      }
      case types.UInt => {
        val nm = this.decl.body.asInstanceOf[UInt].uniname
        AlignMap.put(nm, 4)
        nm
      }
      case types.SInt => {
        val nm = this.decl.body.asInstanceOf[SInt].uniname
        AlignMap.put(nm, 4)
        nm
      }
      case types.Int64 => {
        val nm = this.decl.body.asInstanceOf[Int64].uniname
        AlignMap.put(nm, 8)
        nm
      }
      case types.Int32 => {
        val nm = this.decl.body.asInstanceOf[Int32].uniname
        AlignMap.put(nm, 4)
        nm
      }
      case types.Int16 => {
        val nm = this.decl.body.asInstanceOf[Int16].uniname
        AlignMap.put(nm, 2)
        nm
      }
      case types.Int8 => {
        val nm = this.decl.body.asInstanceOf[Int8].uniname
        AlignMap.put(nm, 1)
        nm
      }
      case _ => throw new Exception("Unsupported object type" + this.decl.typ)
    }
    val ret = PlacementOperator(t + "cacheline", None, None, uniname)
    // PlacementEntityStack.push(ret)
    // add this node to parameter set of SAT solver
    SATParamSet.add(uniname)
    
    ret
  }

  def dcacheline: PlacementOperator = cacheline("d")
  def icacheline: PlacementOperator = cacheline("i")

  def page: PlacementOperator = {
    val uniname = this.decl.typ match {
      case types.BranchInst => {
        val nm = this.decl.body.asInstanceOf[ControlflowInst].uniname
        BaseConstrainSet.add(nm)
        nm
      }
      case types.LoadInst => {
        val nm = this.decl.body.asInstanceOf[LoadInst].uniname
        BaseConstrainSet.add(nm)
        nm
      }
      case types.StoreInst => {
        val nm = this.decl.body.asInstanceOf[StoreInst].uniname
        BaseConstrainSet.add(nm)
        nm
      }
      case types.ExactPadding => this.decl.body.asInstanceOf[ExactPaddingNode].name
      case types.Label => {
        val nm = this.decl.body.asInstanceOf[LabelNode].name
        BaseConstrainSet.add(nm)
        nm
      }
      case types.Func => {
        val nm = this.name
        BaseConstrainSet.add(nm)
        nm
      }
      case types.CacheLine => this.decl.body.asInstanceOf[Cacheline].uniname
      case types.TLBEntry => this.decl.body.asInstanceOf[TLBEntry].uniname
      case types.UInt64 => {
        val nm = this.decl.body.asInstanceOf[UInt64].uniname
        AlignMap.put(nm, 8)
        nm
      }
      case types.UInt32 => {
        val nm = this.decl.body.asInstanceOf[UInt32].uniname
        AlignMap.put(nm, 4)
        nm
      }
      case types.UInt16 => {
        val nm = this.decl.body.asInstanceOf[UInt16].uniname
        AlignMap.put(nm, 2)
        nm
      }
      case types.UInt8 => {
        val nm = this.decl.body.asInstanceOf[UInt8].uniname
        AlignMap.put(nm, 1)
        nm
      }
      case types.Bool => {
        val nm = this.decl.body.asInstanceOf[Bool].uniname
        AlignMap.put(nm, 1)
        nm
      }
      case types.UInt => {
        val nm = this.decl.body.asInstanceOf[UInt].uniname
        AlignMap.put(nm, 4)
        nm
      }
      case types.SInt => {
        val nm = this.decl.body.asInstanceOf[SInt].uniname
        AlignMap.put(nm, 4)
        nm
      }
      case types.Int64 => {
        val nm = this.decl.body.asInstanceOf[Int64].uniname
        AlignMap.put(nm, 8)
        nm
      }
      case types.Int32 => {
        val nm = this.decl.body.asInstanceOf[Int32].uniname
        AlignMap.put(nm, 4)
        nm
      }
      case types.Int16 => {
        val nm = this.decl.body.asInstanceOf[Int16].uniname
        AlignMap.put(nm, 2)
        nm
      }
      case types.Int8 => {
        val nm = this.decl.body.asInstanceOf[Int8].uniname
        AlignMap.put(nm, 1)
        nm
      }
      case _ => throw new Exception("Unsupported object type" + this.decl.typ)
    }
    val ret = PlacementOperator("page", None, None, uniname)
    // PlacementEntityStack.push(ret)
    // add this node to parameter set of SAT solver
    SATParamSet.add(uniname)
    ret
  }
}

object WorldNode {
  def apply(typ: String, body: List[ASTNode], zone: Option[AttackerZones] = None, pidVar: Option[ValueNode] = None): WorldNode = {
    val ret = WorldNode(typ, body, zone, pidVar, true)
    GlobalStack.push(ret)
    ret
  }
}

object PrimitiveNode {
  def apply(name: String, preamble: List[ASTNode], body: List[ASTNode]):ASTNode = {
    val ret = PrimitiveNode(name, preamble, body, true)
    GlobalStack.push(ret)
    ret
  }
}

// name for code generation, decl for permission check
object ValueNode {

}


object DeclareNode {
  def apply(perm: Permission, typ: types, body: ValueBaseTypes, name: String, linkedObj: Option[ObjectValueNode], rule: Option[PlacementNode]): DeclareNode = {
    val ret = DeclareNode(perm, typ, body, name, linkedObj, rule, true)
    GlobalStack.push(ret)
    ret
  }
  def apply_no_push(perm: Permission, typ: types, body: ValueBaseTypes, name: String): DeclareNode = {
    DeclareNode(perm, typ, body, name, None, applied = false)
  }
}

object ObjectDeclareNode {
  def apply(perm: Permission, typ: types, body: ValueBaseTypes, name: String): ObjectDeclareNode = {
    val ret = ObjectDeclareNode(perm, typ, body, name, true)
    GlobalStack.push(ret)
    ret
  }
  def apply_no_push(perm: Permission, typ: types, body: ValueBaseTypes, name: String): ObjectDeclareNode = {
    ObjectDeclareNode(perm, typ, body, name, applied = false)
  }
}

object PlacementDeclNode {
  def apply(name: String, parameters: List[PlacementParameter], statements: List[ASTNode]): PlacementDeclNode = {
    val ret = PlacementDeclNode(name, parameters, statements, true)
    GlobalStack.push(ret)
    ret
  }
  def apply_no_push(name: String, parameters: List[PlacementParameter], statements: List[ASTNode]): PlacementDeclNode = {
    PlacementDeclNode(name, parameters, statements, true)
  }
}


object StatementNode {
  def show (stmt: StatementNode): String = stmt.statement
}

object operatorNode {
}

object ConditionNode {
}

object ElseWhenNode {
}

object OtherwiseNode {
}

object WhenNode {
  /*
  @targetName("copyElsewhen")
  def copy(when: WhenNode, elseWhens: List[ElseWhenNode]): WhenNode = {
    WhenNode(when.condition, when.body, elseWhens, when.otherwiseBody)
  }
  @targetName("copyOtherwise")
  def copy(when: WhenNode, otherwiseBody: OtherwiseNode): WhenNode = {
    WhenNode(when.condition, when.body, when.elseWhens, otherwiseBody)
  }
   */
}

object FunctionNode {
}