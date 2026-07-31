package HT

import HT.ASTNodes.*
import HT.Types.{ControlflowInst, PaddingNode, types}

def While(cond: => ConditionNode, branch: Option[ObjectValueNode] = None)(block: => Any): LoopNode = {
  if (branch.isDefined) {
    if (branch.get.decl.typ != types.BranchInst) {
      throw new Exception("Branch in loop should be a BranchInst")
    }
    FuncAddrConstraintStack.push(branch.get.decl)
    // add to interesting Labels
    InterestingLabels.insert(branch.get.decl.body.asInstanceOf[ControlflowInst].uniname)
  }
  val orig_sp = GlobalStack.size()
  val condNode = cond
  GlobalStack.pop(GlobalStack.size() - orig_sp)
  val localPadding: Option[PaddingNode] = if (PaddingStack.size() > 0 && branch.isDefined) {
    val padding = PaddingStack.top()
    PaddingStack.clear()
    Some(padding)
  } else {
    // if no branch, leave padding for next usage
    None
  }

  block
  val statements = GlobalStack.slice(orig_sp, GlobalStack.size())
  GlobalStack.pop(GlobalStack.size() - orig_sp)



  val ret = LoopNode(condNode, statements.toList, branch, localPadding, AllocUniqueName("loop"));
  GlobalStack.push(ret)
  ret
}

def break(branch: Option[ObjectValueNode] = None) = {
  // break out of current loop
  if (branch.isDefined) {
    if (branch.get.decl.typ != types.BranchInst) {
      throw new Exception("Branch in loop should be a BranchInst")
    }
    FuncAddrConstraintStack.push(branch.get.decl)
    // add to interesting Labels
    InterestingLabels.insert(branch.get.decl.body.asInstanceOf[ControlflowInst].uniname + "_jump")
  }
  val localPadding: Option[PaddingNode] = if (PaddingStack.size() > 0 && branch.isDefined) {
    val padding = PaddingStack.top()
    PaddingStack.clear()
    Some(padding)
  } else {
    // if no branch, leave padding for next usage
    None
  }
  val node = BreakNode(branch, localPadding, AllocUniqueName("break"))
  GlobalStack.push(node)
}
