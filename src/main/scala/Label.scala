package HT

import HT.ASTNodes.{ObjectDeclareNode, ObjectValueNode, PlacementNode}
import HT.Permissions.CurrentDefaultPermission
import HT.Types.{LabelNode, PaddingNode, types}
import sourcecode.Name

object LabelManager {
  var counter = 0
  def getUniqueName(): String = {
    counter += 1
    "label" + counter
  }
}

def Label()(implicit name: Name): ObjectValueNode = {
  //if (rule.isDefined) {
  //  rule2Python(rule.get, name)
  //}
  
  val ret = LabelNode(name.value, None)
  /*
  if (PaddingStack.size() > 0) {
    val padding = PaddingStack.top()
    PaddingStack.clear()
    LabelNode(name, rule, Some(padding))
  } else {

  }*/

  val declNode = ObjectDeclareNode.apply_no_push(CurrentDefaultPermission, types.Label, ret, ret.name)
  //val valueNode = ObjectValueNode(declNode, ret.name)
  GlobalObjectStack.push(declNode)
  //InterestingLabels.insert(ret.name)
  refo(name.value)
}

def PlaceLabel(onode: ObjectValueNode) = {
  if (!onode.decl.body.isInstanceOf[LabelNode]) {
    throw new Exception(s"PlaceLabel param is not a label" + onode.name)
  }
  val node = onode.decl.body.asInstanceOf[LabelNode]
  val ret = if (PaddingStack.size() > 0) {
    val padding = PaddingStack.top()
    PaddingStack.clear()
    LabelNode(node.name, Some(padding))
  } else {
    LabelNode(node.name, None)
  }
  val declNode = ObjectDeclareNode(CurrentDefaultPermission, types.Label, ret, ret.name)
  val valueNode = ObjectValueNode(declNode, ret.name)
  // GlobalObjectStack.push(declNode) // already pushed in Label
  InterestingLabels.insert(ret.name)
  FuncAddrConstraintStack.push(declNode)
}