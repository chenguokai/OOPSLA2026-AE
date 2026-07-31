package HT.ASTNodes

import HT.{ASTNodes, GlobalStack}
import HT.Types.types
import HT.Types.ValueTypes
import HT.Permissions.Permission
import HT.Permissions.CurrentDefaultPermission
import HT.imm

import scala.annotation.targetName

// allow implict conversion from Bool to ConditionNode
given Conversion[ValueNode, ConditionNode] with {
  def apply(v: ValueNode): ConditionNode = {
    if (v.decl.typ != types.Bool) {
      throw new Exception("Cannot convert non-Bool to ConditionNode")
    }
    val conditionStr = "self"
    val toperator = operatorNode(conditionStr)
    ConditionNode(toperator, List(v), v.decl.perm)
  }
}

given Conversion[Long, ValueNode] with {
  def apply(v: Long): ValueNode = {
    imm(v)
  }
}

given Conversion[Int, ValueNode] with {
  def apply(v: Int): ValueNode = {
    imm(v)
  }
}

/*
given Conversion[ValueNode, ObjectValueNode] with {
  def apply(v: ValueNode): ObjectValueNode = {
    if (v.decl.typ != types.Imm) {
      throw new Exception("Cannot convert non-Imm to ObjectValueNode")
    }
    ObjectValueNode(v.decl, v.name)
  }
}
 */

given Conversion[ObjectValueNode, ArithNode] with {
  def apply(v: ObjectValueNode): ArithNode = {
    val toperator = operatorNode("self")
    val toperand = List(v)
    ArithNode(toperator, toperand, v.decl.perm)
  }
}

given Conversion[ValueNode, ArithNode] with {
  def apply(v: ValueNode): ArithNode = {
    //if (v.decl.typ != types.Imm) {
    //  throw new Exception("Cannot convert non-Imm to ArithNode")
    //}
    val toperator = operatorNode("self")
    val operand = List(v)
    val ret = ArithNode(toperator, operand, v.decl.perm)
    //GlobalStack.pop(1)
    GlobalStack.push(ret)
    ret
  }
}

given Conversion[Long, ArithNode] with {
  def apply(v: Long): ArithNode = {
    val vv = imm(v)
    val toperator = operatorNode("self")
    val operand = List(vv)
    val ret = ArithNode(toperator, operand, vv.decl.perm)
    GlobalStack.push(ret)
    ret
  }
}

given Conversion[Int, ArithNode] with {
  def apply(v: Int): ArithNode = {
    val vv = imm(v)
    val toperator = operatorNode("self")
    val operand = List(vv)
    val ret = ArithNode(toperator, operand, vv.decl.perm)
    GlobalStack.push(ret)
    ret
  }
}