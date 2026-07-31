package HT

import HT.Types.*
import HT.ASTNodes.*
import HT.GlobalVarStack.stack
import HT.Permissions.Permission.{VictimPrivate, VictimPublic}
import HT.Permissions.{CurrentDefaultPermission, WorldPermCompatible}
import HT.StdLib.type2instance

import scala.deriving.Mirror
import sourcecode.{Name, Text}

object NameManager {
  protected var currentFunc = "Global"
  def setCurrentFunc(name: String) = {
    currentFunc = name
  }
  def getCurrentFunc() = {
    currentFunc
  }

  def isGlobal: Boolean = currentFunc == "Global"
  def isTrue: Boolean = true
}

object FuncAddrConstraintStack extends StackOperations[ObjectDeclareNode] {
  var prevSize = 0
  def saveContext = {
    prevSize = stack.size
  }
  def restoreContext = {
    stack = stack.slice(0, prevSize)
  }
}

object FuncNodeQueue extends StackOperations[FunctionNode] {
  override def push(node: FunctionNode): Unit = {
    stack = stack :+ node

    // also enforce checks here
    // enforce permission and parameter check here
    // we need to ensure that every function decl has same permission, same parameter numbers and same parameter types
    // we also need to ensure that every function decl has same return type and if any, same rule
    val result = stack.filter(_.name == node.name)
    if (result.size < 2) {
      return
    }
    val name = node.name
    // check if all functions have same permission
    val perms = result.map(_.decl.perm)
    if (perms.distinct.size > 1) {
      throw new Exception("Different permissions for the same function " + name + s" ${perms.distinct.mkString(", ")}")
    }

    // check if all functions have same parameter numbers
    val params = result.map(_.decl.parameters)
    if (params.map(_.size).distinct.size > 1) {
      throw new Exception("Different parameter numbers for the same function " + name + s" ${params.map(_.size).distinct.mkString(", ")}")
    }

    // check if all functions have same parameter types
    val paramTypes = params.map(_.map(_.paramType))
    if (paramTypes.map(_.map(_.toString)).distinct.size > 1) {
      throw new Exception("Different parameter types for the same function " + name + s" ${paramTypes.map(_.map(_.toString)).distinct.mkString(", ")}")
    }

    // check if all functions have same return type
    val retTypes = result.map(_.decl.ret)
    if (retTypes.distinct.size > 1) {
      throw new Exception("Different return types for the same function " + name + s" ${retTypes.distinct.mkString(", ")}")
    }

    // check if all functions have same rule
    val rules = result.map(_.decl.rule)
    if (rules.distinct.size > 1) {
      throw new Exception("Different rules for the same function " + name + s" ${rules.distinct.mkString(", ")}")
    }
  }
  def findDecl(name: String): FunctionNode = {
    val result = stack.filter(_.name == name)
    if (result.isEmpty) {
      throw new Exception("No such variable " + name + " in your scope or your are using g() outside DSL code")
    }


    result.last // return the last one for newest declaration
  } 
}

def call(name: String): FunctionNode = {
  val ret = FuncNodeQueue.findDecl(name)
  WorldPermCompatible(ret.decl.perm) // check if given function is callable from current world
  ret
}

object FunctionParameterStack extends StackOperations[DeclareNode] {
  def findDecl(name: String): DeclareNode = {
    val result = stack.filter(_.name == name)
    if (result.size > 1) {
      throw new Exception("More than one parameters with same name")
    }
    if (result.isEmpty) {
      throw new Exception("No such parameter in your function declaration or you are using v() outside a function")
    }
    result.head
  }
}

case class FuncParameter(name: String, paramType: types)

type Arrow[A] = (String, A)

private def funcTypeOf(value: Any): types = value match {
  case typ: types => typ
  case _: Bool.type => types.Bool
  case _: Addr.type => types.Addr
  case _: SInt.type => types.SInt
  case _: UInt.type => types.UInt
  case _: Int8.type => types.Int8
  case _: Int16.type => types.Int16
  case _: Int32.type => types.Int32
  case _: Int64.type => types.Int64
  case _: UInt8.type => types.UInt8
  case _: UInt16.type => types.UInt16
  case _: UInt32.type => types.UInt32
  case _: UInt64.type => types.UInt64
  case _: Atomic.type => types.Atomic
  case _: Cacheline.type => types.CacheLine
  case other => throw new Exception("Invalid function type " + other + ", expected a Ga.Types.types value or DSL type object")
}

class FunctionBuilder(rule: Option[PlacementNode], retType: types, parameters: List[FuncParameter]):
  def apply(body: => Any)(implicit name: Name): FunctionNode = {
    // construct fake declarations under the name
    // TODO: generate a real one

    //val decl = FunctionDeclNode(name.value, parameters, retType, List(), None, List(), CurrentDefaultPermission)
    //GlobalStack.push(decl)
    
    val objdecl = ObjectDeclareNode(CurrentDefaultPermission, types.Func, Bool.instance("false", name = "func"), name.value)
    GlobalObjectStack.push(objdecl)

    val parm_decls = parameters.map(p => DeclareNode.apply_no_push(CurrentDefaultPermission, p._2, type2instance("0", p._2, Some(CurrentDefaultPermission), p._1), p._1))
    println("parameters " + parm_decls)
    FunctionParameterStack.push(parm_decls)
    val orig_name = NameManager.getCurrentFunc()
    NameManager.setCurrentFunc(name.value)

    GlobalVarStack.saveContext
    // GlobalObjectStack.saveContext
    FuncAddrConstraintStack.saveContext
    val orig_sp = GlobalStack.size()
    body
    val statements = GlobalStack.slice(orig_sp, GlobalStack.size())
    GlobalStack.pop(GlobalStack.size() - orig_sp)

    // extract object decl nodes from GlobalObjectStack
    val object_decls = FuncAddrConstraintStack.slice(FuncAddrConstraintStack.prevSize, FuncAddrConstraintStack.size())
    // filter out cacheline objects
    val object_decls_filtered = object_decls.filter(_.typ != types.CacheLine).toList
    // filter out objects that does not have a rule, objects: Branch, LoadInst, StoreInst
    // _.decl.rule.isDefined for each type
    val object_decls_filtered2 = object_decls_filtered.filter(
      a => a.body match {
        case b: ControlflowInst => true // b.rule.isDefined
        case l: LoadInst => true // l.rule.isDefined
        case s: StoreInst=> true // s.rule.isDefined
        case l: LabelNode => true // l.rule.isDefined
        case i: AsmBlock => true // i.rule.isDefined
        //case j: JmpNode => j.inst.isDefined && j.inst.get.asInstanceOf[BranchInst].rule.isDefined  // pushed branch inst, not itself
        //case c: CallNode => c.inst.isDefined && c.inst.get.asInstanceOf[BranchInst].rule.isDefined // pushed branch inst, not itself
        case p: PaddingNode => false
        case e: ExactPaddingNode => false
        case _ => throw new Exception("Unknown object type within object_decls_filtered2 " + a.body)
      }
    ).toList

    if (object_decls_filtered2.size > 0) {
      // add to interesting Labels
      InterestingLabels.insert(name.value)
    }

    
    NameManager.setCurrentFunc(orig_name)
    GlobalVarStack.restoreContext
    // GlobalObjectStack.restoreContext
    FuncAddrConstraintStack.restoreContext

    val decl = FunctionDeclNode(name.value, parameters, retType, statements.toList, rule, object_decls_filtered2, CurrentDefaultPermission)

    val node = FunctionNode(decl, name.value)

    FuncNodeQueue.push(node)
    //if (rule.isDefined) {
    //  rule2Python(rule.get, name)
    //}

    GlobalStack.pop(1) // pop node that has been pushed onto the stack when we built this FunctionBuilder
    // push to stack
    GlobalStack.push(decl)
    val ret = FunctionNode(decl, name.value)
    FuncNodeQueue.push(ret)
    ret
  }

//(block: => Any)
def Func(rets: types)(args: Arrow[_]*): FunctionBuilder = {
  val parameters = args.map(p => FuncParameter(p._1, funcTypeOf(p._2))).toList
  new FunctionBuilder(None, rets, parameters)
}

def Func(rets: Any)(args: Arrow[_]*): FunctionBuilder = {
  val parameters = args.map(p => FuncParameter(p._1, funcTypeOf(p._2))).toList
  new FunctionBuilder(None, funcTypeOf(rets), parameters)
}
def v(name: String): ValueNode = {
  // return a value node for parameters
  ValueNode(FunctionParameterStack.findDecl(name), name)
}

def ret() = {
  ret_impl(None, None)
}

def ret(branch: Option[ObjectValueNode]) = {
  ret_impl(None, branch)
}

def ret(valueExpr: => ArithNode, branch: Option[ObjectValueNode] = None) = {
  ret_impl(Some(valueExpr), branch)
}

def ret_impl(valueExpr: => Option[ArithNode], branch: Option[ObjectValueNode] = None) = {
  if (branch.isDefined) {
    if (branch.get.decl.typ != types.BranchInst) {
      throw new Exception("Branch object expected")
    }
    InterestingLabels.insert(branch.get.decl.body.asInstanceOf[ControlflowInst].uniname)
  }
  val localPadding: Option[PaddingNode] = if (PaddingStack.size() > 0 && branch.isDefined) {
    val padding = PaddingStack.top()
    PaddingStack.clear()
    Some(padding)
  } else {
    None
  }

  val orig_sp = GlobalStack.size()
  val value: Option[ArithNode] = valueExpr
  GlobalStack.pop(GlobalStack.size() - orig_sp)

  val node = ReturnNode(value, branch, localPadding, AllocUniqueName("return"))
  if (branch.isDefined) {
    FuncAddrConstraintStack.push(branch.get.decl)
  }
  GlobalStack.push(node)
}

def IndirectCall(func: FunctionNode, branch: Option[ObjectValueNode], args: ValueBaseNode*) = {
  if (branch.isDefined) {
    if (branch.get.decl.typ != types.BranchInst) {
      throw new Exception("Branch object expected")
    }
    InterestingLabels.insert(branch.get.decl.body.asInstanceOf[ControlflowInst].uniname)
  }

  val localPadding: Option[PaddingNode] = if (PaddingStack.size() > 0 && branch.isDefined) {
    val padding = PaddingStack.top()
    PaddingStack.clear()
    Some(padding)
  } else {
    // if no padding or no branch for this when, leave padding for next usage
    None
  }

  val node = IndirectCallNode(func, branch, AllocUniqueName("indirect_ptr"), args.toList, localPadding)
  if (branch.isDefined) {
    FuncAddrConstraintStack.push(branch.get.decl)
  }
  GlobalStack.push(node)
}