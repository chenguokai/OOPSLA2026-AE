package HT

import HT.ASTNodes.{PrintIntNode, PrintMultipleNode, ValueNode}
import HT.Types.types

import scala.collection.mutable.ListBuffer

def printInt(num: ValueNode) = {
  // add PrintIntNode to the AST
  val printIntNode = PrintIntNode(num)
  GlobalStack.push(printIntNode)
}

def printMultiple(param: Any*) = {
  // add PrintIntNode to the AST
  var fstr = ""
  var parameters = ListBuffer[ValueNode]()
  for (p <- param) {
    p match {
      case num: ValueNode => {
        num.decl.typ match {
          case types.UInt => {
              fstr += "%u"
              parameters += num
            }
          case types.SInt => {
              fstr += "%d"
              parameters += num
            }
          case types.Bool => {
              fstr += "%d"
              parameters += num
            }
          case types.Int8 => {
              fstr += "%d"
              parameters += num
            }
          case types.Int16 => {
              fstr += "%d"
              parameters += num
            }
          case types.Int32 => {
              fstr += "%d"
              parameters += num
            }
          case types.Int64 => {
              fstr += "%ld"
              parameters += num
            }
          case types.UInt8 => {
              fstr += "%u"
              parameters += num
            }
          case types.UInt16 => {
              fstr += "%u"
              parameters += num
            }
          case types.UInt32 => {
              fstr += "%u"
              parameters += num
            }
          case types.UInt64 => {
              fstr += "%lu"
              parameters += num
            }
          case types.Imm => {
              fstr += "%d"
              parameters += num
            }

        }
      }
      case str: String => fstr += str
      case _ => throw new Exception("Invalid parameter type")
    }
  }
  val printIntNode = PrintMultipleNode(fstr, parameters.toList)
  GlobalStack.push(printIntNode)
}