package HT

import HT.ASTNodes.{ObjectDeclareNode, ObjectValueNode}
import HT.Permissions.CurrentDefaultPermission
import HT.Permissions.Permission.VictimPrivate
import HT.Types.{ControlflowInst, Cacheline, JmpNode, LabelNode, types}

def Jmp(inst: ObjectValueNode, target: ObjectValueNode): JmpNode = {
  // jmp to target (a label) with given branch constraint and optional name, generate reference to this jmp
  if (!(target.decl.body.isInstanceOf[LabelNode])) {
    throw new Exception("Jmp target must be a label")
  }
  val padding = if (PaddingStack.size() == 0) {
    None
  } else {
    Some(PaddingStack.top())
  }
  PaddingStack.clear()
  
  if (!inst.decl.body.isInstanceOf[ControlflowInst]) {
    throw new Exception("Jmp branch must be a branch instruction")
  }
  InterestingLabels.insert(inst.decl.body.asInstanceOf[ControlflowInst].uniname)
  
  val jmp = JmpNode(inst.name, target.decl, Some(inst.decl.body.asInstanceOf[ControlflowInst]), padding)
  val declNode = ObjectDeclareNode(CurrentDefaultPermission, types.Jmp, jmp, AllocUniqueName("jmp"))
  GlobalObjectStack.push(declNode)

  FuncAddrConstraintStack.push(inst.decl)

  jmp
}

def Jmp(target: ObjectValueNode): JmpNode = {
  // jmp to target (a label) with given branch constraint and optional name, generate reference to this jmp
  if (!(target.decl.body.isInstanceOf[LabelNode])) {
    throw new Exception("Jmp target must be a label")
  }
  val padding = if (PaddingStack.size() == 0) {
    None
  } else {
    Some(PaddingStack.top())
  }
  PaddingStack.clear()
  val name = AllocUniqueName("jmp")

  val jmp = JmpNode(name, target.decl, None, padding)
  val declNode = ObjectDeclareNode(CurrentDefaultPermission, types.Jmp, jmp, name)
  GlobalObjectStack.push(declNode)

  jmp
}