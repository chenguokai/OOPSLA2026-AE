package HT

import scala.collection.mutable.Map
import HT.ASTNodes.{ObjectDeclareNode, ObjectValueNode}
import HT.Permissions.CurrentDefaultPermission
import HT.Types.{ExactPaddingNode, PaddingNode, types}

object PaddingStack extends StackOperations[PaddingNode]

object PaddingManager {
  var counter = 0
  def getUniqueName(): String = {
    counter += 1
    "padding" + counter
  }

  var paddingValidMap: Map[String, Long] = Map() // map string to padding size

  def mapAppend(name: String, padding: Long): Unit = {
    if (paddingValidMap.contains(name)) {
      throw new Exception("Padding already exists")
    } else {
      paddingValidMap += (name -> padding)
    }
  }
  def mapFind(name: String): Long = {
    if (paddingValidMap.contains(name)) {
      val ret = paddingValidMap(name)
      if (ret % MarchParameters.NopSize != 0) {
        throw new Exception("Padding size must be multiple of NopSize")
      }
      ret / MarchParameters.NopSize
    } else {
      0 // no usage => no padding
    }
  }

  var paddingSet: Set[String] = Set()
  def setAppend(name: String): Unit = {
    if (paddingSet.contains(name)) {
      throw new Exception("Padding already exists")
    } else {
      paddingSet += name
    }
  }
}

def Padding(): PaddingNode = {
  val ret = PaddingNode(PaddingManager.getUniqueName())
  PaddingManager.paddingSet += ret.name
  val declNode = ObjectDeclareNode(CurrentDefaultPermission, types.Padding, ret, ret.name)
  val valueNode = ObjectValueNode(declNode, ret.name)
  // GlobalStack.push(declNode)
  GlobalObjectStack.push(declNode)
  PaddingStack.push(ret)
  ret
}

def ExactPadding(sz: Long): ExactPaddingNode = {
  val ret = ExactPaddingNode(PaddingManager.getUniqueName(), sz)
  // do not add to paddingSet
  val declNode = ObjectDeclareNode(CurrentDefaultPermission, types.ExactPadding, ret, ret.name)
  val valueNode = ObjectValueNode(declNode, ret.name)
  // GlobalStack.push(declNode)
  GlobalObjectStack.push(declNode)
  // PaddingStack.push(ret)
  ret
}
