package HT.Types

import HT.{AllocUniqueName, GlobalObjectStack, GlobalStack, GlobalVarStack, NameManager, rule2Python}
import HT.ASTNodes.{ArithNode, ArrayNode, ConditionNode, DeclareNode, ObjectDeclareNode, ObjectValueNode, PlacementNode, PullToLocalNode, StatementNode, ValueNode, operatorNode}
import HT.Executors.pyStr2Result
import HT.Permissions.Permission.AttackerPublicRemote
import HT.Permissions.{CurrentDefaultPermission, Permission, WorldPermCompatible}
import sourcecode.{Name, Text}

import scala.util.Try

enum types {
  case Bool, Addr, SInt, UInt, Int8, Int16, Int32, Int64, UInt8, UInt16, UInt32, UInt64, Atomic, Void
  case Imm, Timer
  case CacheLine, CacheBank, TLBEntry, LoadInst, StoreInst, BranchInst, Label, Padding, ExactPadding, Jmp, AsmBlock
  case Func // just for placement
  case Ptr
  case ArrayElement
  case Array
}

var ObjectUsedSet: Set[String] = Set()

object NumericRangeChecker {
  // Define ranges for the types
  val UInt8Bounds = (0L, 255L)
  val SInt8Bounds = (-128L, 127L)
  val UInt16Bounds = (0L, 65535L)
  val SInt16Bounds = (-32768L, 32767L)
  val UInt32Bounds = (0L, 4294967295L)
  val SInt32Bounds = (-2147483648L, 2147483647L)
  val UInt64Bounds = (BigInt(0), BigInt("18446744073709551615")) // 2^64 - 1
  val SInt64Bounds = (BigInt("-9223372036854775808"), BigInt("9223372036854775807"))

  def isWithinRange(s: String): Map[String, Boolean] = {
    def parseNumber(): Option[BigInt] = {
      if (s.matches("^[+-]?0[xX][0-9a-fA-F]+$")) { // Hexadecimal
        if (s(0) == '+' || s(0) == '-') {
          Try(BigInt(s.drop(3), 16)).toOption
        } else {
          Try(BigInt(s.drop(2), 16)).toOption
        }
      } else if (s.matches("^[+-]?\\d+$")) { // Decimal
        Try(BigInt(s)).toOption
      } else None
    }

    parseNumber() match {
      case Some(num) =>
        println(s"Debug num ${num}")
        Map(
          "UInt8" -> (UInt8Bounds._1 <= num && num <= UInt8Bounds._2),
          "SInt8" -> (SInt8Bounds._1 <= num && num <= SInt8Bounds._2),
          "UInt16" -> (UInt16Bounds._1 <= num && num <= UInt16Bounds._2),
          "SInt16" -> (SInt16Bounds._1 <= num && num <= SInt16Bounds._2),
          "UInt32" -> (UInt32Bounds._1 <= num && num <= UInt32Bounds._2),
          "SInt32" -> (SInt32Bounds._1 <= num && num <= SInt32Bounds._2),
          "UInt64" -> (UInt64Bounds._1 <= num && num <= UInt64Bounds._2),
          "SInt64" -> (SInt64Bounds._1 <= num && num <= SInt64Bounds._2)
        )
      case None =>
        Map(
          "UInt8" -> false,
          "SInt8" -> false,
          "UInt16" -> false,
          "SInt16" -> false,
          "UInt32" -> false,
          "SInt32" -> false,
          "UInt64" -> false,
          "SInt64" -> false
        )
    }
  }
}

def isValidNumber(s: String): Boolean = {
  val hexPattern = "^[+-]?0[xX][0-9a-fA-F]+$".r // Hexadecimal format
  val decPattern = "^[+-]?\\d+$".r          // Decimal format

  s match {
    case hexPattern() => true
    case decPattern() => true
    case _            => false
  }
}

sealed trait ValueBaseTypes

sealed trait ValueTypes extends ValueBaseTypes

sealed trait ObjectValueTypes extends ValueBaseTypes // for Addr

sealed trait ValueRefs

case class Timer() extends ValueTypes {
  override def toString: String = "Timer"
}

object Timer {
}

def MakeNVars[R](size: Int, f: Name => R)(implicit nm: Name): List[R] = {
  (0 until size).map { i =>
    // For each element, create a unique name based on the parent variable's name and the index.
    val s = nm.value + "_" + i.toString
    // Create the Name instance to be passed to the function.
    val n: Name = Name(s)

    // Call the function `f` with the initial value, which returns another function.
    // Then, call the returned function with our generated name `n`.
    f(n)
  }.toList
}

case class ArrayElementNode(nm: String, idx: ArithNode) extends ValueTypes

case class HTArray(name: String, size: Int, typ: types) extends ValueTypes

object HTArray {
  def apply(size: Int, typ: types)(implicit nm: Name): ArrayNode = {
    // val initStr = b.toString
    val nname = nm.value // already unique

    val (array, array_name) = {
      (HTArray(nname, size, typ), nname)
    }

    // decl node will be pushed onto the AST stack
    val declNode = {
      DeclareNode(CurrentDefaultPermission, types.Array, array, array_name, None, None)
    }

    val valueNode = ValueNode(declNode, array_name)
    // CHECKME: if we need to place it into GlobalVarStack
    if (NameManager.isTrue)
      GlobalVarStack.push(declNode)
    ArrayNode(array_name, declNode)
  }
}


// Define a custom `Bool` type to represent hardware boolean signals
case class Bool(expr: String, name: String, uniname: String, applied: Boolean) extends ValueTypes {
  override def toString: String = expr

  
}

case class Ptr(src: String, name: String, uniname: String, applied: Boolean) extends ValueTypes {
  override def toString: String = "&" + src
}

object Ptr {
  def apply(src: ValueNode, permission: Option[Permission] = None, name: String, rule: Option[PlacementNode] = None): Unit = {
    val srcStr = src.name
    val nname = AllocUniqueName("ptr")
    val (body, ptr_name) = {
      (Ptr(srcStr, name, nname, true), name)
    }

    val declNode = {
      if (permission.isEmpty) {
        DeclareNode(CurrentDefaultPermission, types.Ptr, body, ptr_name, None, rule)
      } else {
        DeclareNode(permission.get, types.Ptr, body, ptr_name, None, rule)
      }
    }

    if (rule.isDefined && !NameManager.isGlobal) {
      throw new Exception("Cannot set rule for non-global variables")
    }

    //if (rule.isDefined) {
    //  rule2Python(rule.get, nname)
    //}

    val valueNode = ValueNode(declNode, ptr_name)
    if (NameManager.isTrue) {
      GlobalVarStack.push(declNode)
    }
  }
}

case class Atomic(expr: String, uniname: String) extends ValueTypes {
  override def toString: String = expr
}

object Atomic {
  def apply(init: Any, permission: Permission = CurrentDefaultPermission, rule: Option[PlacementNode] = None)(implicit name: Name): ValueNode = {
    val initStr = init.match {
      case i: Int => i.toString
      case s: String => s
      case _: Any => throw new Exception("Invalid init value for SInt")
    }
    if (!isValidNumber(initStr)) {
      throw new Exception("Invalid init value for Atomic " + initStr + ", should be a number")
    }
    if (!NumericRangeChecker.isWithinRange(initStr)("SInt32")) {
      // will declare as atomic_int
      throw new Exception("Invalid init value for Atomic " + initStr + ", out of range")
    }
    val nname = AllocUniqueName("atomic")
    val (body, atomic_name) = {
      (Atomic(initStr, nname), name.value)
    }
    //if (rule.isDefined) {
    //  rule2Python(rule.get, nname)
    //}

    // decl node will be pushed onto the AST stack
    val declNode = {
      DeclareNode(permission, types.Atomic, body, atomic_name, None, rule)
    }
    val valueNode = ValueNode(declNode, atomic_name)
    if (NameManager.isTrue) {
      GlobalVarStack.push(declNode)
    }
    valueNode
  }

  def gen(init: Any)(implicit name: Name): ValueNode = {
    apply(init, CurrentDefaultPermission, None)
  }

  def instance(init: Any, permission: Option[Permission] = None, name: String): Atomic = {
    val initStr = init match {
      case i: Int => i.toString
      case s: String => s
      case _: Any => throw new Exception("Invalid init value for SInt")
    }
    if (!isValidNumber(initStr)) {
      throw new Exception("Invalid init value for SInt " + initStr + ", should be a number")
    }
    if (!NumericRangeChecker.isWithinRange(initStr)("SInt32")) {
      throw new Exception("Invalid init value for SInt " + initStr + ", out of range")
    }
    val nname = AllocUniqueName("sint")
    val (body, atomic_name) = {
      (Atomic(initStr, nname), name)
    }
    body
  }
}

object Bool {
  def apply(init: Any, permission: Permission = CurrentDefaultPermission, rule: Option[PlacementNode] = None)(implicit name: Name): ValueNode = {
    val initStr = init match {
      case b: Boolean => b.toString
      case i: Int => if (i == 0) "false" else "true"
      case s: String => s
      case _: Any => throw new Exception("Invalid init value for Bool")
    }
    if (!(initStr == "true" || initStr == "false")) {
      throw new Exception("Invalid init value for Bool " + initStr + ", should be either true or false")
    }
    // val initStr = b.toString
    val nname = AllocUniqueName("bool")

    if (NameManager.isGlobal) {
      val objdecl = ObjectDeclareNode(CurrentDefaultPermission, types.Bool, Bool("0", nname, nname, false), name.value)
      GlobalObjectStack.push(objdecl)
    }

    val (body, bool_name) = {
      (Bool(initStr, name.value, nname, true), name.value)
    }
    
    // decl node will be pushed onto the AST stack
    val declNode = {
      DeclareNode(permission, types.Bool, body, bool_name, None, rule)
    }
    //if (rule.isDefined) {
    //  rule2Python(rule.get, nname)
    //}
    val valueNode = ValueNode(declNode, bool_name)
    if (NameManager.isTrue)
      GlobalVarStack.push(declNode)
    valueNode
  }

  def gen(init: Any)(implicit name: Name): ValueNode = {
    apply(init, CurrentDefaultPermission, None)
  }

  def instance(init: Any, permission: Option[Permission] = None, name: String): Bool = {
    val initStr = init match {
      case b: Boolean => b.toString
      case i: Int => i.toString
      case s: String => s
      case _: Any => throw new Exception("Invalid init value for Bool")
    }
    if (!(initStr == "true" || initStr == "false" || initStr == "")) {
      throw new Exception("Invalid init value for Bool " + initStr + ", should be either true or false")
    }
    val nname = AllocUniqueName("bool")
    val (body, bool_name) = {
      (Bool(initStr, name, nname, true), name)
    }
    body
  }
}


// TODO: think about what role Addr plays in our DSL
case class Addr(expr: String) extends ObjectValueTypes {
  override def toString: String = expr
}

object Addr {
  def apply(init: Any, permission: Permission = CurrentDefaultPermission)(implicit name: Name): Unit = {
    val initStr = init match {
      case i: Int => i.toString
      case s: String => s
      case _: Any => throw new Exception("Invalid init value for Addr")
    }
    if (!isValidNumber(initStr)) {
      throw new Exception("Invalid init value for Addr " + initStr + ", should be a number")
    }
    val (body, addr_name) = {
      (Addr(initStr), name.value)
    }

    // decl node will be pushed onto the AST stack
    val declNode = {
      ObjectDeclareNode(permission, types.Addr, body, addr_name)
    }
    val valueNode = ObjectValueNode(declNode, addr_name)
    if (NameManager.isTrue)
      GlobalObjectStack.push(declNode)
    // valueNode
  }
  def instance(init: Any, permission: Option[Permission] = None, name: String): Addr = {
    val initStr = init match {
      case i: Int => i.toString
      case s: String => s
      case _: Any => throw new Exception("Invalid init value for Addr")
    }
    if (!isValidNumber(initStr)) {
      throw new Exception("Invalid init value for Addr " + initStr + ", should be a number")
    }
    val (body, addr_name) = {
      (Addr(initStr), name)
    }
    body
  }
}

// Internal use only, do not expose to the user
private[HT] case class Imm(expr: String) extends ValueTypes {
  override def toString: String = expr
}

private[HT] object Imm {
  def apply(init: Any, perm: Option[Permission] = None, name: Option[String] = None): ValueNode = {
    // Never generate any decl node for Imm
    val initStr = init match {
      case i: Int => i.toString
      case s: String => s
      case _: Any => throw new Exception("Invalid init value for Imm")
    }
    if (!isValidNumber(initStr)) {
      throw new Exception("Invalid init value for Imm " + initStr + ", should be a number")
    }
    val (body, imm_name) = {
      if (name.isEmpty) {
        val nname = AllocUniqueName("imm")
        (Imm(initStr), nname)
      } else {
        (Imm(initStr), name.get)
      }
    }

    // decl node will be pushed onto the AST stack
    val declNode = {
      if (perm.isEmpty) {
        DeclareNode.apply_no_push(CurrentDefaultPermission, types.Imm, body, imm_name)
      } else {
        DeclareNode.apply_no_push(perm.get, types.Imm, body, imm_name)
      }
    }
    val valueNode = ValueNode(declNode, imm_name)
    //GlobalStack.push(valueNode)
    valueNode
  }
}


case class SInt(expr: String, uniname: String) extends ValueTypes {
  override def toString: String = expr
}

object SInt {
  def apply(init: Any, permission: Permission = CurrentDefaultPermission, rule: Option[PlacementNode] = None)(implicit name: Name): ValueNode = {
    val initStr = init.match {
      case i: Int => i.toString
      case s: String => s
      case _: Any => throw new Exception("Invalid init value for SInt")
    }
    if (!isValidNumber(initStr)) {
      throw new Exception("Invalid init value for SInt " + initStr + ", should be a number")
    }
    if (!NumericRangeChecker.isWithinRange(initStr)("SInt32")) {
      throw new Exception("Invalid init value for SInt " + initStr + ", out of range")
    }
    val nname = AllocUniqueName("sint")

    if (NameManager.isGlobal) {
      val objdecl = ObjectDeclareNode(CurrentDefaultPermission, types.SInt, SInt("0", nname), name.value)
      GlobalObjectStack.push(objdecl)
    }
    
    val (body, sint_name) = {
      (SInt(initStr, nname), name.value)
    }
    //if (rule.isDefined) {
    //  rule2Python(rule.get, nname)
    //}
    
    // decl node will be pushed onto the AST stack
    val declNode = {
      DeclareNode(permission, types.SInt, body, sint_name, None, rule)
    }
    val valueNode = ValueNode(declNode, sint_name)
    if (NameManager.isTrue)
      GlobalVarStack.push(declNode)
    valueNode
  }

  def gen(init: Any)(implicit name: Name): ValueNode = {
    apply(init, CurrentDefaultPermission, None)
  }

  def instance(init: Any, permission: Option[Permission] = None, name: String): SInt = {
    val initStr = init match {
      case i: Int => i.toString
      case s: String => s
      case _: Any => throw new Exception("Invalid init value for SInt")
    }
    if (!isValidNumber(initStr)) {
      throw new Exception("Invalid init value for SInt " + initStr + ", should be a number")
    }
    if (!NumericRangeChecker.isWithinRange(initStr)("SInt32")) {
      throw new Exception("Invalid init value for SInt " + initStr + ", out of range")
    }
    val nname = AllocUniqueName("sint")
    val (body, sint_name) = {
      (SInt(initStr, nname), name)
    }
    body
  }
}

case class UInt(expr: String, uniname: String) extends ValueTypes {
  override def toString: String = expr
}

object UInt {
  def apply(init: Any, permission: Permission = CurrentDefaultPermission, rule: Option[PlacementNode] = None)(implicit name: Name): ValueNode = {
    val initStr = init match {
      case i: Int => i.toString
      case s: String => s
      case _: Any => throw new Exception("Invalid init value for UInt")
    }
    if (!isValidNumber(initStr)) {
      throw new Exception("Invalid init value for UInt " + initStr + ", should be a number")
    }
    if (!NumericRangeChecker.isWithinRange(initStr)("UInt32")) {
      throw new Exception("Invalid init value for UInt " + initStr + ", out of range")
    }
    val nname = AllocUniqueName("uint")

    if (NameManager.isGlobal) {
      val objdecl = ObjectDeclareNode(CurrentDefaultPermission, types.UInt, UInt("0", nname), name.value)
      GlobalObjectStack.push(objdecl)
    }
    
    val (body, uint_name) = {
      (UInt(initStr, nname), name.value)
    }
    //if (rule.isDefined) {
    //  rule2Python(rule.get, nname)
    //}
    
    // decl node will be pushed onto the AST stack
    val declNode = {
      DeclareNode(permission, types.UInt, body, uint_name, None, rule)
    }
    val valueNode = ValueNode(declNode, uint_name)
    if (NameManager.isTrue)
      GlobalVarStack.push(declNode)
    valueNode
  }

  def gen(init: Any)(implicit name: Name): ValueNode = {
    apply(init, CurrentDefaultPermission, None)
  }

  def instance(init: Any, permission: Option[Permission] = None, name: String): UInt = {
    val initStr = init match {
      case i: Int => i.toString
      case s: String => s
      case _: Any => throw new Exception("Invalid init value for UInt")
    }
    if (!isValidNumber(initStr)) {
      throw new Exception("Invalid init value for UInt " + initStr + ", should be a number")
    }
    if (!NumericRangeChecker.isWithinRange(initStr)("UInt32")) {
      throw new Exception("Invalid init value for UInt " + initStr + ", out of range")
    }
    val nname = AllocUniqueName("uint")
    val (body, uint_name) = {
      (UInt(initStr, nname), name)
    }
    body
  }
}

case class Int8(expr: String, uniname: String) extends ValueTypes {
  override def toString: String = expr
}

object Int8 {
  def apply(init: Any, permission: Permission = CurrentDefaultPermission, rule: Option[PlacementNode] = None)(implicit name: Name): ValueNode = {
    val initStr = init match {
      case i: Int => i.toString
      case s: String => s
      case _: Any => throw new Exception("Invalid init value for Int8")
    }
    if (!isValidNumber(initStr)) {
      throw new Exception("Invalid init value for Int8 " + initStr + ", should be a number")
    }
    if (!NumericRangeChecker.isWithinRange(initStr)("SInt8")) {
      throw new Exception("Invalid init value for Int8 " + initStr + ", out of range")
    }
    val nname = AllocUniqueName("int8")

    if (NameManager.isGlobal) {
      val objdecl = ObjectDeclareNode(CurrentDefaultPermission, types.Int8, Int8("0", nname), name.value)
      GlobalObjectStack.push(objdecl)
    }
    
    val (body, int8_name) = {
      (Int8(initStr, nname), name.value)
    }
    //if (rule.isDefined) {
    //  rule2Python(rule.get, nname)
    //}
    
    // decl node will be pushed onto the AST stack
    val declNode = {
      DeclareNode(permission, types.Int8, body, int8_name, None, rule)
    }
    val valueNode = ValueNode(declNode, int8_name)
    if (NameManager.isTrue)
      GlobalVarStack.push(declNode)
    valueNode
  }

  def gen(init: Any)(implicit name: Name): ValueNode = {
    apply(init, CurrentDefaultPermission, None)
  }

  def instance(init: Any, permission: Option[Permission] = None, name: String): Int8 = {
    val initStr = init match {
      case i: Int => i.toString
      case s: String => s
      case _: Any => throw new Exception("Invalid init value for Int8")
    }
    if (!isValidNumber(initStr)) {
      throw new Exception("Invalid init value for Int8 " + initStr + ", should be a number")
    }
    if (!NumericRangeChecker.isWithinRange(initStr)("SInt8")) {
      throw new Exception("Invalid init value for Int8 " + initStr + ", out of range")
    }
    val nname = AllocUniqueName("int8")
    val (body, int8_name) = {
      (Int8(initStr, nname), name)
    }
    body
  }
}

case class Int16(expr: String, uniname: String) extends ValueTypes {
  override def toString: String = expr
}

object Int16 {
  def apply(init: Any, permission: Permission = CurrentDefaultPermission, rule: Option[PlacementNode] = None)(implicit name: Name): ValueNode = {
    val initStr = init match {
      case i: Int => i.toString
      case s: String => s
      case _: Any => throw new Exception("Invalid init value for Int16")
    }
    if (!isValidNumber(initStr)) {
      throw new Exception("Invalid init value for Int16 " + initStr + ", should be a number")
    }
    if (!NumericRangeChecker.isWithinRange(initStr)("SInt16")) {
      throw new Exception("Invalid init value for Int16 " + initStr + ", out of range")
    }
    val nname = AllocUniqueName("int16")

    if (NameManager.isGlobal) {
      val objdecl = ObjectDeclareNode(CurrentDefaultPermission, types.Int16, Int16("0", nname), name.value)
      GlobalObjectStack.push(objdecl)
    }
    
    val (body, int16_name) = {
      (Int16(initStr, nname), name.value)
    }
    //if (rule.isDefined) {
    //  rule2Python(rule.get, nname)
    //}
    
    // decl node will be pushed onto the AST stack
    val declNode = {
      DeclareNode(permission, types.Int16, body, int16_name, None, rule)
    }
    val valueNode = ValueNode(declNode, int16_name)
    if (NameManager.isTrue)
      GlobalVarStack.push(declNode)
    valueNode
  }

  def gen(init: Any)(implicit name: Name): ValueNode = {
    apply(init, CurrentDefaultPermission, None)
  }

  def instance(init: Any, permission: Option[Permission] = None, name: String): Int16 = {
    val initStr = init match {
      case i: Int => i.toString
      case s: String => s
      case _: Any => throw new Exception("Invalid init value for Int16")
    }
    if (!isValidNumber(initStr)) {
      throw new Exception("Invalid init value for Int16 " + initStr + ", should be a number")
    }
    if (!NumericRangeChecker.isWithinRange(initStr)("SInt16")) {
      throw new Exception("Invalid init value for Int16 " + initStr + ", out of range")
    }
    val nname = AllocUniqueName("int16")
    val (body, int16_name) = {
      (Int16(initStr, nname), name)
    }
    body
  }
}

case class Int32(expr: String, uniname: String) extends ValueTypes {
  override def toString: String = expr
}

object Int32 {
  def apply(init: Any, permission: Permission = CurrentDefaultPermission, rule: Option[PlacementNode] = None)(implicit name: Name): ValueNode = {
    val initStr = init match {
      case i: Int => i.toString
      case s: String => s
      case _: Any => throw new Exception("Invalid init value for Int32")
    }
    if (!isValidNumber(initStr)) {
      throw new Exception("Invalid init value for Int32 " + initStr + ", should be a number")
    }
    if (!NumericRangeChecker.isWithinRange(initStr)("SInt32")) {
      throw new Exception("Invalid init value for Int32 " + initStr + ", out of range")
    }
    val nname = AllocUniqueName("int32")

    if (NameManager.isGlobal) {
      val objdecl = ObjectDeclareNode(CurrentDefaultPermission, types.Int32, Int32("0", nname), name.value)
      GlobalObjectStack.push(objdecl)
    }
    
    val (body, int32_name) = {
      (Int32(initStr, nname), name.value)
    }
    //if (rule.isDefined) {
    //  rule2Python(rule.get, nname)
    //}
    
    // decl node will be pushed onto the AST stack
    val declNode = {
      DeclareNode(permission, types.Int32, body, int32_name, None, rule)
    }
    val valueNode = ValueNode(declNode, int32_name)
    if (NameManager.isTrue)
      GlobalVarStack.push(declNode)
    valueNode
  }

  def gen(init: Any)(implicit name: Name): ValueNode = {
    apply(init, CurrentDefaultPermission, None)
  }

  def instance(init: Any, permission: Option[Permission] = None, name: String): Int32 = {
    val initStr = init match {
      case i: Int => i.toString
      case s: String => s
      case _: Any => throw new Exception("Invalid init value for Int32")
    }
    if (!isValidNumber(initStr)) {
      throw new Exception("Invalid init value for Int32 " + initStr + ", should be a number")
    }
    if (!NumericRangeChecker.isWithinRange(initStr)("SInt32")) {
      throw new Exception("Invalid init value for Int32 " + initStr + ", out of range")
    }
    val nname = AllocUniqueName("int32")
    val (body, int32_name) = {
      (Int32(initStr, nname), name)
    }
    body
  }
}

case class Int64(expr: String, uniname: String) extends ValueTypes {
  override def toString: String = expr
}

object Int64 {
  def apply(init: Any, permission: Permission = CurrentDefaultPermission, rule: Option[PlacementNode] = None)(implicit name: Name): ValueNode = {
    val initStr = init match {
      case i: Int => i.toString
      case s: String => s
      case _: Any => throw new Exception("Invalid init value for Int64")
    }
    if (!isValidNumber(initStr)) {
      throw new Exception("Invalid init value for Int64 " + initStr + ", should be a number")
    }
    if (!NumericRangeChecker.isWithinRange(initStr)("SInt64")) {
      throw new Exception("Invalid init value for Int64 " + initStr + ", out of range")
    }
    val nname = AllocUniqueName("int64")

    if (NameManager.isGlobal) {
      val objdecl = ObjectDeclareNode(CurrentDefaultPermission, types.Int64, Int64("0", nname), name.value)
      GlobalObjectStack.push(objdecl)
    }
    
    val (body, int64_name) = {
      (Int64(initStr, nname), name.value)
    }
    //if (rule.isDefined) {
    //  rule2Python(rule.get, nname)
    //}

    // decl node will be pushed onto the AST stack
    val declNode = {
      DeclareNode(permission, types.Int64, body, int64_name, None, rule)
    }
    val valueNode = ValueNode(declNode, int64_name)
    if (NameManager.isTrue)
      GlobalVarStack.push(declNode)
    valueNode
  }

  def gen(init: Any)(implicit name: Name): ValueNode = {
    apply(init, CurrentDefaultPermission, None)
  }

  def instance(init: Any, permission: Option[Permission] = None, name: String): Int64 = {
    val initStr = init match {
      case i: Int => i.toString
      case s: String => s
      case _: Any => throw new Exception("Invalid init value for Int64")
    }
    if (!isValidNumber(initStr)) {
      throw new Exception("Invalid init value for Int64 " + initStr + ", should be a number")
    }
    if (!NumericRangeChecker.isWithinRange(initStr)("SInt64")) {
      throw new Exception("Invalid init value for Int64 " + initStr + ", out of range")
    }
    val nname = AllocUniqueName("int64")
    val (body, int64_name) = {
      (Int64(initStr, nname), name)
    }
    body
  }
}

case class UInt8(expr: String, uniname: String) extends ValueTypes {
  override def toString: String = expr
}

object UInt8 {
  def apply(init: Any, permission: Permission = CurrentDefaultPermission, rule: Option[PlacementNode] = None)(implicit name: Name): ValueNode = {
    val initStr = init match {
      case i: Int => i.toString
      case s: String => s
      case _: Any => throw new Exception("Invalid init value for UInt8")
    }
    if (!isValidNumber(initStr)) {
      throw new Exception("Invalid init value for UInt8 " + initStr + ", should be a number")
    }
    if (!NumericRangeChecker.isWithinRange(initStr)("UInt8")) {
      throw new Exception("Invalid init value for UInt8 " + initStr + ", out of range")
    }
    val nname = AllocUniqueName("uint8")
    
    if (NameManager.isGlobal) {
      val objdecl = ObjectDeclareNode(CurrentDefaultPermission, types.UInt8, UInt8("0", nname), name.value)
      GlobalObjectStack.push(objdecl)
    }
    
    val (body, uint8_name) = {
      (UInt8(initStr, nname), name.value)
    }
    //if (rule.isDefined) {
    //  rule2Python(rule.get, nname)
    //}

    // decl node will be pushed onto the AST stack
    val declNode = {
      DeclareNode(permission, types.UInt8, body, uint8_name, None, rule)
    }
    val valueNode = ValueNode(declNode, uint8_name)
    if (NameManager.isTrue)
      GlobalVarStack.push(declNode)
    valueNode
  }

  def gen(init: Any)(implicit name: Name): ValueNode = {
    apply(init, CurrentDefaultPermission, None)
  }

  def instance(init: Any, permission: Option[Permission] = None, name: String): UInt8 = {
    val initStr = init match {
      case i: Int => i.toString
      case s: String => s
      case _: Any => throw new Exception("Invalid init value for UInt8")
    }
    if (!isValidNumber(initStr)) {
      throw new Exception("Invalid init value for UInt8 " + initStr + ", should be a number")
    }
    if (!NumericRangeChecker.isWithinRange(initStr)("UInt8")) {
      throw new Exception("Invalid init value for UInt8 " + initStr + ", out of range")
    }
    val nname = AllocUniqueName("uint8")
    val (body, uint8_name) = {
      (UInt8(initStr, nname), name)
    }
    body
  }
}

case class UInt16(expr: String, uniname: String) extends ValueTypes {
  override def toString: String = expr
}

object UInt16 {
  def apply(init: Any, permission: Permission = CurrentDefaultPermission, rule: Option[PlacementNode] = None)(implicit name: Name): ValueNode = {
    val initStr = init match {
      case i: Int => i.toString
      case s: String => s
      case _: Any => throw new Exception("Invalid init value for UInt16")
    }
    if (!isValidNumber(initStr)) {
      throw new Exception("Invalid init value for UInt16 " + initStr + ", should be a number")
    }
    if (!NumericRangeChecker.isWithinRange(initStr)("UInt16")) {
      throw new Exception("Invalid init value for UInt16 " + initStr + ", out of range")
    }
    val nname = AllocUniqueName("uint16")

    if (NameManager.isGlobal) {
      val objdecl = ObjectDeclareNode(CurrentDefaultPermission, types.UInt16, UInt16("0", nname), name.value)
      GlobalObjectStack.push(objdecl)
    }
    
    val (body, uint16_name) = {
      (UInt16(initStr, nname), name.value)
    }
    //if (rule.isDefined) {
    //  rule2Python(rule.get, nname)
    //}

    // decl node will be pushed onto the AST stack
    val declNode = {
      DeclareNode(permission, types.UInt16, body, uint16_name, None, rule)
    }
    val valueNode = ValueNode(declNode, uint16_name)
    if (NameManager.isTrue)
      GlobalVarStack.push(declNode)
    valueNode
  }

  def gen(init: Any)(implicit name: Name): ValueNode = {
    apply(init, CurrentDefaultPermission, None)
  }

  def instance(init: Any, permission: Option[Permission] = None, name: String): UInt16 = {
    val initStr = init match {
      case i: Int => i.toString
      case s: String => s
      case _: Any => throw new Exception("Invalid init value for UInt16")
    }
    if (!isValidNumber(initStr)) {
      throw new Exception("Invalid init value for UInt16 " + initStr + ", should be a number")
    }
    if (!NumericRangeChecker.isWithinRange(initStr)("UInt16")) {
      throw new Exception("Invalid init value for UInt16 " + initStr + ", out of range")
    }
    val nname = AllocUniqueName("uint16")
    val (body, uint16_name) = {
      (UInt16(initStr, nname), name)
    }
    body
  }
}

case class UInt32(expr: String, uniname: String) extends ValueTypes {
  override def toString: String = expr
}

object UInt32 {
  def apply(init: Any, permission: Permission = CurrentDefaultPermission)(implicit name: Name): ValueNode = {
    val initStr = init match {
      case i: Int => i.toString
      case s: String => s
      case _: Any => throw new Exception("Invalid init value for UInt32")
    }
    if (!isValidNumber(initStr)) {
      throw new Exception("Invalid init value for UInt32 " + initStr + ", should be a number")
    }
    if (!NumericRangeChecker.isWithinRange(initStr)("UInt32")) {
      throw new Exception("Invalid init value for UInt32 " + initStr + ", out of range")
    }
    val nname = AllocUniqueName("uint32")

    if (NameManager.isGlobal) {
      val objdecl = ObjectDeclareNode(CurrentDefaultPermission, types.UInt32, UInt32("0", nname), name.value)
      GlobalObjectStack.push(objdecl)
    }
    
    val (body, uint32_name) = {
      (UInt32(initStr, nname), name.value)
    }
    //if (rule.isDefined) {
    //  rule2Python(rule.get, nname)
    //}

    // decl node will be pushed onto the AST stack
    val declNode = {
      DeclareNode(permission, types.UInt32, body, uint32_name, None, None)
    }
    val valueNode = ValueNode(declNode, uint32_name)
    if (NameManager.isTrue)
      GlobalVarStack.push(declNode)
    valueNode
  }

  def gen(init: Any)(implicit name: Name): ValueNode = {
    apply(init, CurrentDefaultPermission)
  }

  def instance(init: Any, permission: Option[Permission] = None, name: String): UInt32 = {
    val initStr = init match {
      case i: Int => i.toString
      case s: String => s
      case _: Any => throw new Exception("Invalid init value for UInt32")
    }
    if (!isValidNumber(initStr)) {
      throw new Exception("Invalid init value for UInt32 " + initStr + ", should be a number")
    }
    if (!NumericRangeChecker.isWithinRange(initStr)("UInt32")) {
      throw new Exception("Invalid init value for UInt32 " + initStr + ", out of range")
    }
    val nname = AllocUniqueName("uint32")
    val (body, uint32_name) = {
      (UInt32(initStr, nname), name)
    }
    body
  }
}

case class UInt64(expr: String, uniname: String) extends ValueTypes {
  override def toString: String = expr
}

object UInt64 {
  def apply(init: Any, permission: Permission = CurrentDefaultPermission)(implicit name: Name): ValueNode = {
    val initStr = init match {
      case i: Int => i.toString
      case l: Long => l.toString
      case s: String => s
      case _: Any => throw new Exception("Invalid init value for UInt64")
    }
    if (!isValidNumber(initStr)) {
      throw new Exception("Invalid init value for UInt64 " + initStr + ", should be a number")
    }
    if (!NumericRangeChecker.isWithinRange(initStr)("UInt64")) {
      throw new Exception("Invalid init value for UInt64 " + initStr + ", out of range")
    }
    val nname = AllocUniqueName("uint64")

    if (NameManager.isGlobal) {
      val objdecl = ObjectDeclareNode(CurrentDefaultPermission, types.UInt64, UInt64("0", nname), name.value)
      GlobalObjectStack.push(objdecl)
    }

    val (body, uint64_name) = {
      (UInt64(initStr, nname), name.value)
    }
    //if (rule.isDefined) {
    //  rule2Python(rule.get, nname)
    //}

    // decl node will be pushed onto the AST stack
    val declNode = {
      DeclareNode(permission, types.UInt64, body, uint64_name, None, None)
    }
    val valueNode = ValueNode(declNode, uint64_name)
    if (NameManager.isTrue)
     GlobalVarStack.push(declNode)
    valueNode
  }

  def gen(init: Any)(implicit name: Name): ValueNode = {
    apply(init, CurrentDefaultPermission)
  }

  def instance(init: Any, permission: Option[Permission] = None, name: String): UInt64 = {
    val initStr = init match {
      case i: Int => i.toString
      case s: String => s
      case _: Any => throw new Exception("Invalid init value for UInt64")
    }
    if (!(initStr == "" || isValidNumber(initStr))) {
      throw new Exception("Invalid init value for UInt64 " + initStr + ", should be a number")
    }
    if (!(initStr == "" || NumericRangeChecker.isWithinRange(initStr)("UInt64"))) {
      throw new Exception("Invalid init value for UInt64 " + initStr + ", out of range")
    }
    val nname = AllocUniqueName("uint64")
    val (body, uint64_name) = {
      (UInt64(initStr, nname), name)
    }
    body
  }
}


sealed trait ObjectTypes extends ValueBaseTypes

case class PaddingNode(name: String) extends ObjectTypes

case class ExactPaddingNode(name: String, sz: Long) extends ObjectTypes

case class JmpNode(name: String, target: ObjectDeclareNode, inst: Option[ControlflowInst], padding: Option[PaddingNode]) extends ObjectTypes

case class LabelNode(name: String, padding: Option[PaddingNode]) extends ObjectTypes

case class Cacheline(rule: PlacementNode, name: String, uniname: String,/* addr: Long,*/ applied: Boolean) extends ObjectTypes

object Cacheline {
  def apply(rules: PlacementNode, name: String): Unit = {
    println("Cacheline apply method")
    // compute rule in this method
    // for cacheline, we only need single parameter
    val uniname = AllocUniqueName("cacheline")
    //val pythonCode = rule2Python(rules, uniname)

    val (decl, decl_name) = //if (name.isEmpty) {
    (Cacheline(rules, name, uniname, true), name)
    println("current stack size " + GlobalStack.size())
    val declNode = {
      ObjectDeclareNode(CurrentDefaultPermission, types.CacheLine, decl, decl_name)
    }
    println("post stack size " + GlobalStack.size())

    val valueNode = ObjectValueNode(declNode, decl_name)
    if (NameManager.isTrue)
      GlobalObjectStack.push(declNode)
    // valueNode
  }
}

case class TLBEntry(rule: PlacementNode, name: String, uniname: String, applied: Boolean) extends ObjectTypes

object TLBEntry {
  def apply(rules: PlacementNode, perm: Option[Permission] = None, name: String): Unit = {
    val uniname = AllocUniqueName("tlbentry")
    //val pythonCode = rule2Python(rules, uniname)

    val (decl, decl_name) = (TLBEntry(rules, name, uniname, true), name)

    val declNode = if(perm.isEmpty) {
      ObjectDeclareNode(CurrentDefaultPermission, types.TLBEntry, decl, decl_name)
    } else {
      ObjectDeclareNode(perm.get, types.TLBEntry, decl, decl_name)
    }

    val valueNode = ObjectValueNode(declNode, decl_name)
    if (NameManager.isTrue)
      GlobalObjectStack.push(declNode)
  }
}

case class TLBPermission(R: Boolean, W: Boolean, X: Boolean) {
  def toProtString: String = {
    val parts = List(
      if (R) Some("PROT_READ") else None,
      if (W) Some("PROT_WRITE") else None,
      if (X) Some("PROT_EXEC") else None
    ).flatten

    if (parts.isEmpty) "(PROT_NONE)"
    else s"(${parts.mkString(" | ")})"
  }
}

case class ControlflowInst(name: String, uniname: String, applied: Boolean) extends ObjectTypes

object ControlflowInst {
  def apply(perm: Option[Permission] = None)(implicit name: Name): ObjectValueNode = {
    println("Branch apply method")
    val uniname = AllocUniqueName("branch")
    //if (rules.isDefined) {
    //  rule2Python(rules.get, uniname)
    //}
    val (decl, decl_name) = {
        (ControlflowInst(name.value, uniname, true), name.value)
    }
    ObjectUsedSet += uniname
    println("current stack size " + GlobalStack.size())
    val declNode = if (perm.isEmpty) {
      ObjectDeclareNode(CurrentDefaultPermission, types.BranchInst, decl, decl_name)
    } else {
      ObjectDeclareNode(perm.get, types.BranchInst, decl, decl_name)
    }
    println("post stack size " + GlobalStack.size())

    val valueNode = ObjectValueNode(declNode, decl_name)
    if (NameManager.isTrue)
      GlobalObjectStack.push(declNode)
    valueNode
  }
}

case class AsmBlock(name: String, uniname: String, applied: Boolean) extends ObjectTypes

object AsmBlock {
  def apply()(implicit name: Name): ObjectValueNode = {
    val uniname = AllocUniqueName("asm")
    val (decl, decl_name) = (AsmBlock(name.value, uniname, true), name.value)
    val declNode = ObjectDeclareNode(CurrentDefaultPermission, types.AsmBlock, decl, decl_name)
    val valueNode = ObjectValueNode(declNode, decl_name)
    if (NameManager.isTrue) {
      GlobalObjectStack.push(declNode)
    }
    valueNode
  }
}

case class LoadInst(name: String, uniname: String, applied: Boolean) extends ObjectTypes

object LoadInst {
  def apply(perm: Option[Permission] = None)(implicit name: Name): ObjectValueNode = {
    val uniname = AllocUniqueName("load")
    //if (rules.isDefined) {
    //  rule2Python(rules.get, uniname)
    //}
    val (decl, decl_name) = {
      (LoadInst(name.value, uniname, true), name.value)
    }
    ObjectUsedSet += uniname
    val declNode = if (perm.isEmpty) {
      ObjectDeclareNode(CurrentDefaultPermission, types.LoadInst, decl, decl_name)
    } else {
      ObjectDeclareNode(perm.get, types.LoadInst, decl, decl_name)
    }
    val valueNode = ObjectValueNode(declNode, decl_name)
    if (NameManager.isTrue)
      GlobalObjectStack.push(declNode)
    valueNode
  }
}

case class StoreInst(name: String, uniname: String, applied: Boolean) extends ObjectTypes

object StoreInst {
  def apply(perm: Option[Permission] = None)(implicit name: Name): ObjectValueNode = {
    val uniname = AllocUniqueName("store")
    //if (rules.isDefined) {
    //  rule2Python(rules.get, uniname)
    //}
    val (decl, decl_name) = (StoreInst(name.value, uniname, true), name.value)
    ObjectUsedSet += uniname
    val declNode = if (perm.isEmpty) {
      ObjectDeclareNode(CurrentDefaultPermission, types.StoreInst, decl, decl_name)
    } else {
      ObjectDeclareNode(perm.get, types.StoreInst, decl, decl_name)
    }
    val valueNode = ObjectValueNode(declNode, decl_name)
    if (NameManager.isTrue)
      GlobalObjectStack.push(declNode)
    valueNode
  }
}

object Inst {
  def ControlFlow (perm: Option[Permission] = None)(implicit name: Name): ObjectValueNode = ControlflowInst(perm)
  def Load (perm: Option[Permission] = None)(implicit name: Name): ObjectValueNode = LoadInst(perm)
  def Store (perm: Option[Permission] = None)(implicit name: Name): ObjectValueNode = StoreInst(perm)
}

def PullToLocal(pid: ValueNode, local: ValueNode, remote: ValueNode) = {
  // Pull remote value and write to local value
  if (remote.decl.perm != AttackerPublicRemote) {
    throw new Exception("PullToLocal: remote variable permission is not public remote")
  }
  // permission check inside
  WorldPermCompatible(local.decl.perm)
  WorldPermCompatible(pid.decl.perm)
  
  val node = PullToLocalNode(pid, local, remote)
  GlobalStack.push(node)
}