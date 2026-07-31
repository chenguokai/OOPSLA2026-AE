package HT

import HT.Types.*
import HT.ASTNodes.*
import HT.{codeGenInternal, codeGenWithMap}
import HT.CodeGen.{CodeType2Z3, Z3BodyPostfix, Z3BodyPrefix, Z3Postfix, Z3Prefix}
import HT.Executors.pyStr2Result
import HT.Permissions.CurrentDefaultPermission

import scala.collection.mutable.ListBuffer
import scala.deriving.Mirror
import scala.collection.mutable.Map

case class PlacementParameter(name: String, paramType: types)

var UniqueNameAddrMap: Map[String, Long] = Map() // map unique name to address, filled after SMT solver returns

// type Arrow[A] = (String, A) // already defined in function
trait StackOperations[T] {
  var stack: ListBuffer[T] = ListBuffer()

  def clear(): Unit = stack.clear()

  def slice(start: Int, end: Int): ListBuffer[T] = stack.slice(start, end)

  def pop(count: Int): Unit = {
    stack = stack.dropRight(count)
  }

  def push(node: T): Unit = {
    stack = stack :+ node
  }

  def top(): T = stack.last

  def size(): Int = stack.size

  def push(nodes: List[T]): Unit = {
    stack ++= nodes
  }
}

// Now you can use it like this:
object PlacementParameterStack extends StackOperations[DeclareNode] {
  def findDecl(name: String): DeclareNode = {
    val result = stack.filter(_.name == name)
    if (result.size > 1) {
      throw new Exception("More than one parameters with same name")
    }
    if (result.isEmpty) {
      throw new Exception("No such parameter in your placement rule declaration or your are using a() outside a placement rule")
    }
    result.head
  }
}


class PlacementBuilder(name: String, parameters: List[PlacementParameter]):
  def apply(body: => Any):PlacementNode = {
    // construct decl nodes for our parameter and push them onto the stack
    // these decl nodes should only act as a type reference, not real outputs

    val parm_decls = parameters.map(p => DeclareNode.apply_no_push(CurrentDefaultPermission, p._2, Addr(""), p._1)).toList
    PlacementParameterStack.push(parm_decls)

    val orig_sp = GlobalStack.size()
    body
    val statements = GlobalStack.slice(orig_sp, GlobalStack.size())
    println("Placement body " + statements)
    GlobalStack.pop(GlobalStack.size() - orig_sp)

    // parm decls will not be used any more
    // C do not support recursive function so we are safe to clear here
    PlacementParameterStack.clear()

    val decl = PlacementDeclNode(name, parameters, statements.toList)
    //GlobalStack.push(decl) // already pushed in PlacementDeclNode
    //val pythonCode = rule2Python(PlacementNode(decl, name), )
    //println(s"Debug python code for ${name}:\n " + pythonCode)
    //val result = pyStr2Result(pythonCode)
    //val addr = result.split(" = ").last.trim.toLong
    //val addr = // compute the address now
    PlacementNode(decl, name)
  }



def placement(name: String)(args: Arrow[_]*): PlacementBuilder = {
  println("PlacementBuilder")
  val parameters = args.map(p => PlacementParameter(p._1, p._2.asInstanceOf[types])).toList
  new PlacementBuilder(name, parameters)
}



def a(name: String):ValueNode = {
  // return a value node, for parameters' reference
  val ret = ValueNode(PlacementParameterStack.findDecl(name), name)
  //GlobalStack.push(ret) // need to push because operator will pop it
  ret
}

def imm(num: Long): ValueNode = {
  // return a value node, for immediate value's reference
  val decl = DeclareNode.apply_no_push(CurrentDefaultPermission, types.Imm, body = Imm(num.toString), num.toString)
  val ret = ValueNode(decl, num.toString)
  //GlobalStack.push(ret) // need to push because operator will pop it
  ret
}

var PlacementParamString = ""
var PlacementBodyString = ""

def applyNameToRule(rules: PlacementNode, args: List[String]): String = {
  // setup a map between args and args in PlacementNode
  if (rules.decl.parameters.size != args.size) {
    throw new Exception("Placement rule's parameter size does not match")
  }
  val argMap = rules.decl.parameters.zip(args).map{case (k, v) => (k.name, v)}.toMap.to(collection.mutable.Map)
  // may cause duplication decl of param, handled before running python code
  val param = rules.decl.parameters.map(p => s"${argMap(p.name)} = ${CodeType2Z3(p.paramType)}('${argMap(p.name)}') ").mkString("\n")
  val body = rules.decl.body.map(a => codeGenWithMap(a, argMap)).mkString(",")
  // PlacementParamString += param + "\n"
  PlacementBodyString += body + ","
  ""
}

def rule2Python(rule: PlacementNode, uniname: String): String = {
  if (rule.decl.parameters.size != 1) {
    throw new Exception("Placement rule only support exactly one parameter")
  }
  applyNameToRule(rule, List(uniname))
  ""
}

/*
def BaseRule2Python(orig: Long, rule: PlacementNode): String = {
  val param = rule.decl.parameters.map(p => s"${p.name} = ${CodeType2Z3(p.paramType)}('${p.name}') ").mkString("\n")
  val body = s"orig == ${orig}," + rule.decl.body.map(codeGenInternal).mkString(",")
  // Z3Prefix() + param + "\n" + Z3BodyPrefix() + body + Z3BodyPostfix() + Z3Postfix()
  PlacementParamString += param
  PlacementBodyString += body
  ""
}
*/

def ruleFromAddr(addr: Long): PlacementNode = {
  // construct a rule from an address that maps to the rule
  //val decl = PlacementDeclNode.apply_no_push("ruleFromAddr", List(), List())
  //val ret = PlacementNode(decl, "ruleFromAddr", addr)
  val rule = placement(AllocUniqueName("localrule"))("orig" -> types.Addr) {
    a("orig") == imm(addr)
  }
  rule
}

def ParamFromSet() = {
  // remove duplication of parameters
  PlacementParamString = SATParamSet.map{
    a => s"${a} = ${CodeType2Z3(types.Addr)}('${a}') "
  }.mkString("\n")

}

def rules2Python(): String = {
  // construct python code for all rules
  Z3Prefix() + PlacementParamString + "\n" + Z3BodyPrefix() + PlacementBodyString + Z3BodyPostfix() + Z3Postfix()
}

def GlobalSolve() = {
  addConstrainToPython()
  ParamFromSet()
  val pythonCode = rules2Python()
  UniqueNameAddrMap.clear()
  println(pythonCode)
  UniqueNameAddrMap = pyStr2Result(pythonCode)
}

def recordUsedObjects(node: PlacementOperator): Unit = {
  node.op match {
    case "self" | "saddr" | "imm" | "dcacheline" | "icacheline" | "page" => {
      if (ObjectUsedSet.contains(node.uniname)) {
        ObjectUsedSet -= node.uniname
      }
    }
    case _ => {
      if (node.left.isDefined) {
        recordUsedObjects(node.left.get)
      }
      if (node.right.isDefined) {
        recordUsedObjects(node.right.get)
      }
    }
  }
}

def AppendConstraint(node: PlacementOperator) = {
  PlacementEntityStack.push(node)
  recordUsedObjects(node)
}

sealed trait PlacementEntity

case class PlacementOperator(op: String, left: Option[PlacementOperator], right: Option[PlacementOperator], uniname: String) extends PlacementEntity  {
  def <(that: PlacementOperator): PlacementOperator = {
    // PlacementEntityStack.pop(2)
    val ret = PlacementOperator("<", Some(this), Some(that), "")
    // PlacementEntityStack.push(ret)
    ret
  }

  def >(that: PlacementOperator): PlacementOperator = {
    // PlacementEntityStack.pop(2)
    val ret = PlacementOperator(">", Some(this), Some(that), "")
    // PlacementEntityStack.push(ret)
    ret
  }

  def <=(that: PlacementOperator): PlacementOperator = {
      // PlacementEntityStack.pop(2)
      val ret = PlacementOperator("<=", Some(this), Some(that), "")
      // PlacementEntityStack.push(ret)
      ret
    }

  def +(that: PlacementOperator): PlacementOperator = {
    // PlacementEntityStack.pop(2)
    val ret = PlacementOperator("+", Some(this), Some(that), "")
    // PlacementEntityStack.push(ret)
    ret
  }

  def -(that: PlacementOperator): PlacementOperator = {
    // PlacementEntityStack.pop(2)
    val ret = PlacementOperator("-", Some(this), Some(that), "")
    // PlacementEntityStack.push(ret)
    ret
  }

  def *(that: PlacementOperator): PlacementOperator = {
    // PlacementEntityStack.pop(2)
    val ret = PlacementOperator("*", Some(this), Some(that), "")
    // PlacementEntityStack.push(ret)
    ret
  }

  def /(that: PlacementOperator): PlacementOperator = {
    // PlacementEntityStack.pop(2)
    val ret = PlacementOperator("/", Some(this), Some(that), "")
    // PlacementEntityStack.push(ret)
    ret
  }

  def %(that: PlacementOperator): PlacementOperator = {
    // PlacementEntityStack.pop(2)
    val ret = PlacementOperator("%", Some(this), Some(that), "")
    // PlacementEntityStack.push(ret)
    ret
  }

  def &(that: PlacementOperator): PlacementOperator = {
    // PlacementEntityStack.pop(2)
    val ret = PlacementOperator("&", Some(this), Some(that), "")
    // PlacementEntityStack.push(ret)
    ret
  }

  def |(that: PlacementOperator): PlacementOperator = {
    // PlacementEntityStack.pop(2)
    val ret = PlacementOperator("|", Some(this), Some(that), "")
    // PlacementEntityStack.push(ret)
    ret
  }

  def ^(that: PlacementOperator): PlacementOperator = {
    // PlacementEntityStack.pop(2)
    val ret = PlacementOperator("^", Some(this), Some(that), "")
    // PlacementEntityStack.push(ret)
    ret
  }

  def ===(that: PlacementOperator): PlacementOperator = {
    // PlacementEntityStack.pop(2)
    val ret = PlacementOperator("===", Some(this), Some(that), "")
    // PlacementEntityStack.push(ret)
    ret
  }

  def =/= (that: PlacementOperator): PlacementOperator = {
    // PlacementEntityStack.pop(2)
    val ret = PlacementOperator("!=", Some(this), Some(that), "")
    // PlacementEntityStack.push(ret)
    ret
  }

  def &&(that: PlacementOperator): PlacementOperator = {
    // PlacementEntityStack.pop(2)
    val ret = PlacementOperator("&&", Some(this), Some(that), "")
    // PlacementEntityStack.push(ret)
    ret
  }

  def ||(that: PlacementOperator): PlacementOperator = {
    // PlacementEntityStack.pop(2)
    val ret = PlacementOperator("||", Some(this), Some(that), "")
    // PlacementEntityStack.push(ret)
    ret
  }
  
  def >>(that: PlacementOperator): PlacementOperator = {
    // PlacementEntityStack.pop(2)
    val ret = PlacementOperator(">>", Some(this), Some(that), "")
    // PlacementEntityStack.push(ret)
    ret
  }
  
  def in(that: PlacementOperator): PlacementOperator = {
    // PlacementEntityStack.pop(2)
    val ret = PlacementOperator("in", Some(this), Some(that), "")
    // PlacementEntityStack.push(ret)
    ret
  }
}

def BitFold(x: PlacementOperator, width: Int, length: Int): PlacementOperator = {
  // count the number of 1s in the binary representation of x
  val ret = PlacementOperator("BitFold", Some(x), None, "")
  ret
}

object PlacementEntityStack extends StackOperations[PlacementOperator]

case class PlacementConstrain(uniname: String, name: String, statements: List[PlacementOperator]) extends PlacementEntity {
  override def toString: String = {
    val ret = statements.map(_.toString).mkString(" && ")
    s"constraint $name: $ret"
  }
}

object PlacementConstrainStack extends StackOperations[PlacementConstrain]

// convert imm to PlacementOperator
given Conversion[ValueNode, PlacementOperator] with {
  def apply(num: ValueNode): PlacementOperator = {
    if (num.decl.typ != types.Imm) {
      throw new Exception("Only immediate value can be converted to PlacementOperator")
    }
    val ret = PlacementOperator("imm", None, None, num.decl.body.asInstanceOf[Imm].expr)
    // PlacementEntityStack.push(ret)
    ret
  }
}

given Conversion[Long, PlacementOperator] with {
  def apply(num: Long): PlacementOperator = {
    val v = imm(num)
    val ret = PlacementOperator("imm", None, None, v.decl.body.asInstanceOf[Imm].expr)
    // PlacementEntityStack.push(ret)
    ret
  }
}

given Conversion[Int, PlacementOperator] with {
  def apply(num: Int): PlacementOperator = {
    val v = imm(num)
    val ret = PlacementOperator("imm", None, None, v.decl.body.asInstanceOf[Imm].expr)
    // PlacementEntityStack.push(ret)
    ret
  }
}

// add rules among different objects
def Constrain(name: String = "")(body: => Any) = {
  // construct constrains among different objects
  val orig_sp = PlacementEntityStack.size()
  body
  val statements = PlacementEntityStack.slice(orig_sp, PlacementEntityStack.size())
  println("Placement body " + statements)
  if (statements.isEmpty) {
    throw new Exception("Constrain body is empty")
  }
  PlacementEntityStack.pop(PlacementEntityStack.size() - orig_sp)
  val uniname = AllocUniqueName("Constraint")
  val nodename = if (name == "") {
    uniname
  } else {
    name
  }
  val node = PlacementConstrain(uniname, nodename, statements.toList)
  PlacementConstrainStack.push(node)
}

def printConstrains: String = {
  val ret = PlacementConstrainStack.slice(0, PlacementConstrainStack.size()).map(_.toString).mkString("\n")
  ret
}

def SMTGen(a: PlacementOperator): String = {
  // convert PlacementOperator to SMT code
  val ret: String = a.op match {
    case "self" => a.uniname
    case "saddr" => a.uniname
    case "imm" => a.uniname
    case "+" => {
      if (a.left.isEmpty || a.right.isEmpty) {
        throw new Exception("Left or right operand is empty")
      }
      // TODO: ensure that left and right are of same type
      s"(${SMTGen(a.left.get)} + ${SMTGen(a.right.get)})"
    }
    case "-" => {
      if (a.left.isEmpty || a.right.isEmpty) {
        throw new Exception("Left or right operand is empty")
      }
      // TODO: ensure that left and right are of same type
      s"(${SMTGen(a.left.get)} - ${SMTGen(a.right.get)})"
    }
    case "*" => {
      if (a.left.isEmpty || a.right.isEmpty) {
        throw new Exception("Left or right operand is empty")
      }
      // TODO: ensure that left and right are of same type
      s"(${SMTGen(a.left.get)} * ${SMTGen(a.right.get)})"
    }
    case "/" => {
      if (a.left.isEmpty || a.right.isEmpty) {
        throw new Exception("Left or right operand is empty")
      }
      // TODO: ensure that left and right are of same type
      s"(${SMTGen(a.left.get)} / ${SMTGen(a.right.get)})"
    }
    case "%" => {
      if (a.left.isEmpty || a.right.isEmpty) {
        throw new Exception("Left or right operand is empty")
      }
      // TODO: ensure that left and right are of same type
      s"(${SMTGen(a.left.get)} % ${SMTGen(a.right.get)})"
    }
    case "&&" => s"(${SMTGen(a.left.get)} && ${SMTGen(a.right.get)})"
    case "||" => s"(${SMTGen(a.left.get)} || ${SMTGen(a.right.get)})"
    case "===" => s"(${SMTGen(a.left.get)} == ${SMTGen(a.right.get)})"
    case ">>" => s"(${SMTGen(a.left.get)} >> ${SMTGen(a.right.get)})"
    case "!=" => s"(${SMTGen(a.left.get)} != ${SMTGen(a.right.get)})"
    case "<" => s"(${SMTGen(a.left.get)} < ${SMTGen(a.right.get)})"
    case ">" => s"(${SMTGen(a.left.get)} > ${SMTGen(a.right.get)})"
    case "&" => s"(${SMTGen(a.left.get)} & ${SMTGen(a.right.get)})"
    case "|" => s"(${SMTGen(a.left.get)} | ${SMTGen(a.right.get)})"
    case "^" => s"(${SMTGen(a.left.get)} ^ ${SMTGen(a.right.get)})"
    case "!=" => s"(${SMTGen(a.left.get)} != ${SMTGen(a.right.get)})"
    case "<=" => s"(${SMTGen(a.left.get)} <= ${SMTGen(a.right.get)})"
    case "dcacheline" => {
      s"${a.uniname} / ${MarchParameters.L1DLine}"
    }
    case "icacheline" => {
      s"${a.uniname} / ${MarchParameters.L1ILine}"
    }
    case "page" => {
      s"${a.uniname} / ${MarchParameters.PageSize}"
    }
    case "BitFold" => {
      s"""
         |${SMTGen(a.left.get)}
         |""".stripMargin
      /*
      s"""
         |(lambda xb:
         |    Concat(*[
         |        reduce(xor, (Extract(i, i, xb) for i in range(start, 48, 2)))
         |        for start in (13, 12)
         |    ])
         |)(Int2BV(${SMTGen(a.left.get)}, 64))
         |""".stripMargin
       */
    }
    case "in" => {
      if (!(a.left.get.op == "dcacheline" || a.left.get.op == "icacheline" || a.left.get.op == "saddr")) {
        throw new Exception("Left operand should be either cacheline or saddr")
      }
      if (!(a.right.get.op == "dcacheline" || a.right.get.op == "icacheline" || a.right.get.op == "page")) {
        throw new Exception("Left operand should be cacheline or page")
      }
      if (a.right.get.op == "dcacheline") {
        s"${a.left.get.uniname} >= ${a.right.get.uniname}, ${a.left.get.uniname} ${if (a.left.get.op == "saddr") " < " else s" + ${MarchParameters.L1DLine} <= " } ${a.right.get.uniname} + ${MarchParameters.L1DLine}"
      } else if (a.right.get.op == "page") {
        s"${a.left.get.uniname} >= ${a.right.get.uniname}, ${a.left.get.uniname} ${if (a.left.get.op == "saddr") " < " else s" + ${MarchParameters.L1DLine} <= " } ${a.right.get.uniname} + ${MarchParameters.PageSize}"
      } else if (a.right.get.op == "icacheline") {
        s"${a.left.get.uniname} >= ${a.right.get.uniname}, ${a.left.get.uniname} ${if (a.left.get.op == "saddr") " < " else s" + ${MarchParameters.L1ILine} <= " } ${a.right.get.uniname} + ${MarchParameters.L1ILine}"
      } else {
        throw new Exception("Right operand should be cacheline or page")
      }
    }
    case _ => throw new Exception("Unsupported operator " + a.op)
  }
  ret
}

def constrain2Python(node: PlacementConstrain) = {
  // with constrain, we are not refering to any new objects
  // only add constrain expressions to body
  val body = node.statements.map{
    a => SMTGen(a)
  }
  PlacementBodyString += body.mkString(",") + ","
  // rules from AlignMap
  if (!AlignMap.isEmpty) {
    val alignString = AlignMap.map {
      case (k, v) => s"${k} % ${v} == 0,${k} >= 0x20000000,${k} < ${if (MarchParameters.ISA == "riscv64") 0x70000000L else 0x100000000000L}"
    }.mkString(",")
    PlacementBodyString += alignString + ","
  }
  if (!BaseConstrainSet.isEmpty) {
    val baseString = BaseConstrainSet.map {
      a => s"${a} >= 0,${a} <= 0xFFFFFFFFFFFFFFFF"
    }.mkString(",")
    PlacementBodyString += baseString + ","
  }
}

def addConstrainToPython() = {
  PlacementConstrainStack.slice(0, PlacementConstrainStack.size()).map{
    // for each constrain, add it to the python code
    case node: PlacementConstrain => {
      constrain2Python(node)
    }
  }
}