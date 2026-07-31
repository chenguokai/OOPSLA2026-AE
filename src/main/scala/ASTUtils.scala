package HT.ASTUtils

import HT.ASTNodes.{ASTNode, AssignNode, CallNode, ConditionNode, DeclareNode, FunctionDeclNode, LoopNode, ObjectDeclareNode, PlacementDeclNode, PlacementNode, WhenNode, WorldNode}
import HT.Types.{Addr, Bool, Cacheline, ValueBaseTypes}
import HT.UniqueNameAddrMap

// traverse the AST and filter out non-victim world code
def filterNonVictimWorldCode(ast: ASTNode): Option[ASTNode] = {
  ast match {
    case node: WorldNode =>
      if (node.typ == "Victim") {
        // recursively check the body
        val newBody = node.body.flatMap(filterNonVictimWorldCode)
        Some(WorldNode(node.typ, newBody, None, None, node.applied))
      } else {
        None
      }
    case node: DeclareNode =>
      Some(node)
    // TODO: add intermediate node types
    case node: PlacementDeclNode => {
      Some(node)
    }
    case node: ObjectDeclareNode => {
      Some(node)
    }
    case node: FunctionDeclNode => {
      val newBody = node.body.flatMap(filterNonVictimWorldCode)
      Some(FunctionDeclNode(node.name, node.parameters, node.ret, newBody, node.rule, node.placementObjects, node.perm))
    }
    case node: LoopNode => {
      val newBody = node.body.flatMap(filterNonVictimWorldCode)
      Some(LoopNode(node.condition, newBody, node.branch, node.padding, node.uniname))
    }
    case node: AssignNode => {
      Some(node)
    }

    case _ => {
      throw new Exception("Unexpected ASTNode type " + ast)
      None
    }
  }
}

def filterNonVictimControlWorldCode(ast: ASTNode): Option[ASTNode] = {
  ast match {
    case node: WorldNode =>
      if (node.typ == "Victim" || node.typ == "Control") {
        // recursively check the body
        val newBody = node.body.flatMap(filterNonVictimControlWorldCode)
        Some(WorldNode(node.typ, newBody, None, None, node.applied))
      } else {
        None
      }
    case node: DeclareNode =>
      Some(node)
    // TODO: add intermediate node types
    case _ => {
      throw new Exception("Unexpected ASTNode type " + ast)
      None
    }
  }
}

def printValueBase(types: ValueBaseTypes, i: Int): Unit = {
  types match {
    case node: Bool =>
      println("  " * i + "Bool: " + node.expr)
    case node: Addr =>
      println("  " * i + "Addr: " + node.expr)
    case node: Cacheline =>
      println("  " * i + "Cacheline: " + node.name)
      println("  " * i + "  Rule:")
      printAST(node.rule, i + 2)
    case _ =>
      println("  " * i + types)
  }
}

// Better printing for AST
def printAST(ast: ASTNode, indent: Int = 0): Unit = {
  // println("  " * indent + ast)
  ast match {
    case node: WorldNode =>
      println("  " * indent + "World Node:" + node.typ)
      println("  " * (indent + 1) + "Body:")
      node.body.foreach(printAST(_, indent + 2))
    case node: DeclareNode =>
      println("  " * indent + "Declare Node:" + node.name)
      println("  " * (indent + 1) + "Type:" + node.typ)
      println("  " * (indent + 1) + "Permission:" + node.perm)
      println("  " * (indent + 1) + "Body:")
      //println("  " * (indent + 1) + "  " + node.body)
      printValueBase(node.body, indent + 1)
      if (node.linkedObj.isDefined) {
        println("  " * (indent + 1) + "Linked Object:")
        printAST(node.linkedObj.get, indent + 2)
      }
    case node: PlacementDeclNode =>
      println("  " * indent + "PlacementDecl Node:" + node.name)
      println("  " * (indent + 1) + "parameters: " + node.parameters)
      println("  " * (indent + 1) + "body:")
      node.body.foreach(printAST(_, indent + 1))
    case node: ConditionNode =>
      println("  " * indent + "Condition Node:")
      println("  " * (indent + 1) + "Operator: " + node.toperator)
      println("  " * (indent + 1) + "Operands:")
      node.body.foreach(printAST(_, indent + 1))
    case node: FunctionDeclNode =>
      println("  " * indent + "FunctionDecl Node:" + node.name)
      if (node.rule.isDefined) {
        println("  " * (indent + 1) + "Rule:")
        printAST(node.rule.get, indent + 2)
      }
      println("  " * (indent + 1) + "parameters: " + node.parameters)
      println("  " * (indent + 1) + "return type: " + node.ret)
      if (node.placementObjects.nonEmpty) {
        println("  " * (indent + 1) + "Placement Objects:")
        node.placementObjects.foreach(printAST(_, indent + 2))
      }
      println("  " * (indent + 1) + "body:")
      node.body.foreach(printAST(_, indent + 2))
    case node: CallNode =>
      println("  " * indent + "Call Node:")
      println("  " * (indent + 1) + "Function: ")
      printAST(node.func, indent + 1)
      println("  " * (indent + 1) + "Arguments:")
      node.args.foreach(printAST(_, indent + 2))
    case node: WhenNode =>
      println("  " * indent + "When Node:")
      println("  " * (indent + 1) + "Condition:")
      printAST(node.condition, indent + 2)
      println("  " * (indent + 1) + "Body:")
      node.body.foreach(printAST(_, indent + 2))
      println("  " * (indent + 1) + "Else Whens:")
      node.elseWhens.foreach(printAST(_, indent + 2))
      println("  " * (indent + 1) + "Otherwise Body:")
      printAST(node.otherwiseBody, indent + 2)
    case node: LoopNode =>
      println("  " * indent + "Loop Node:")
      println("  " * (indent + 1) + "Condition:")
      printAST(node.condition, indent + 2)
      println("  " * (indent + 1) + "Body:")
      node.body.foreach(printAST(_, indent + 2))
    case node: PlacementNode => {
      println("  " * (indent + 1) + "Placement Node: " + node.name)
      println("  " * (indent + 1) + "Decl:")
      printAST(node.decl, indent + 2)

      // deprecated, cannot retrieve the addr from the name at this stage
      // println("  " * (indent + 1) + "Addr: " + UniqueNameAddrMap(node.name))
    }
    case _ =>
      println("  " * indent + ast)
  }
}