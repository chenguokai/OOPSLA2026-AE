package HT

import HT.ASTNodes.ObjectValueNode
import HT.Types.{LabelNode, PaddingNode, types}

def InstUniqueName(inst: ObjectValueNode): String = {
  if (inst.decl.typ != types.Label || !inst.decl.body.isInstanceOf[LabelNode]) {
    throw new Exception("Instruction handle must be a Label object " + inst.name)
  }
  inst.decl.body.asInstanceOf[LabelNode].name
}

def PrepareInstHandle(inst: Option[ObjectValueNode]): (Option[PaddingNode], String) = {
  inst match {
    case Some(handle) =>
      val uniname = InstUniqueName(handle)
      InterestingLabels.insert(uniname)
      FuncAddrConstraintStack.push(handle.decl)
      val padding = if (PaddingStack.size() == 0) {
        None
      } else {
        Some(PaddingStack.top())
      }
      PaddingStack.clear()
      (padding, uniname)
    case None =>
      (None, AllocUniqueName("unlabeled_inst"))
  }
}
