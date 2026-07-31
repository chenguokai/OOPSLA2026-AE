package HT

import HT.ASTNodes._
import scala.collection.mutable.ListBuffer

object GlobalVarStack extends StackOperations[DeclareNode] {
  var prevSize = 0
  def findDecl(name: String): DeclareNode = {
    val result = stack.filter(_.name == name)
    if (result.isEmpty) {
      throw new Exception("No such variable " + name + " in your scope or your are using g() outside DSL code")
    }
    result.last // return the last one for newest declaration
  }
  def saveContext = {
    prevSize = stack.size
  }
  def restoreContext = {
    stack = stack.dropRight(stack.size - prevSize)
  }
}

def refv(name: String): ValueNode = {
  ValueNode(GlobalVarStack.findDecl(name), name)
}

object GlobalObjectStack extends StackOperations[ObjectDeclareNode] {
  var prevSize = 0
  def findDecl(name: String): ObjectDeclareNode = {
    val result = stack.filter(_.name == name)
    if (result.isEmpty) {
      throw new Exception("No such global object " + name + " in your declaration or your are using go() outside DSL code")
    }
    result.last // return the last one for newest declaration
  }
  def saveContext = {
    prevSize = stack.size
  }
  def restoreContext = {
    stack = stack.dropRight(stack.size - prevSize)
  }
}

def refo(name: String): ObjectValueNode = {
  ObjectValueNode(GlobalObjectStack.findDecl(name), name)
}

def refo(name: String, count: Int): ObjectValueNode = {
  ObjectValueNode(GlobalObjectStack.findDecl(name + s"_$count"), name + s"_$count")
}