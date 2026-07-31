package HT

import HT.ASTNodes.*
import HT.Permissions.CurrentDefaultPermission
import HT.Types.{LoadInst, StoreInst, types}

def Load_impl(src: ValueNode, dst: ValueNode, inst: Option[ObjectValueNode] = None): Unit = {
  if (inst.isDefined) {
    // add to interesting Labels
    InterestingLabels.insert(inst.get.decl.body.asInstanceOf[LoadInst].uniname)
    FuncAddrConstraintStack.push(inst.get.decl)
    println("Appended")
  }
  // use top of padding stack to add padding to the load
  if (inst.isDefined) {

    val padding = if (PaddingStack.size() == 0) {
      None
    } else {
      Some(PaddingStack.top())
    }
    val load = LoadNode(src, dst, inst, padding, inst.get.decl.body.asInstanceOf[LoadInst].uniname)
    
    GlobalStack.push(load)
    PaddingStack.clear()
  } else {
    val load = LoadNode(src, dst, inst, None, AllocUniqueName("unlabeled_load"))
    
    GlobalStack.push(load)

    // PaddingStack.clear()
  }
}

def Load(src: ValueNode, dst: ValueNode) = {
  Load_impl(src, dst, None)
}

def Load(src: ValueNode, dst: ValueNode, inst: ObjectValueNode) = {
  assert(inst.decl.typ == types.LoadInst)
  Load_impl(src, dst, Some(inst))
}

def Store_impl(src: ValueNode, dst: ValueNode, inst: Option[ObjectValueNode] = None, noderef: Boolean = false): Unit = {
  if (inst.isDefined) {
    // add to interesting Labels
    InterestingLabels.insert(inst.get.decl.body.asInstanceOf[StoreInst].uniname)

  }
  if (inst.isDefined) {
    val padding = if (PaddingStack.size() == 0) {
      None
    } else {
      Some(PaddingStack.top())
    }
    val store = StoreNode(src, dst, inst, padding, inst.get.decl.body.asInstanceOf[StoreInst].uniname, noderef)

    GlobalStack.push(store)
    FuncAddrConstraintStack.push(inst.get.decl)
    PaddingStack.clear()
  } else {
    val store = StoreNode(src, dst, inst, None, AllocUniqueName("unlabeled_store"), noderef)
    
    GlobalStack.push(store)

    // PaddingStack.clear()
  }
}

def StoreRef(src: ValueNode, dst: ValueNode) = {
  Store_impl(src, dst, None, true)
}

def StoreRef(src: ValueNode, dst: ValueNode, inst: ObjectValueNode) = {
  assert(inst.decl.typ == types.StoreInst)
  Store_impl(src, dst, Some(inst), true)
}

def Store(src: ValueNode, dst: ValueNode) = {
  Store_impl(src, dst, None)
}

def Store(src: ValueNode, dst: ValueNode, inst: ObjectValueNode) = {
  assert(inst.decl.typ == types.StoreInst)
  Store_impl(src, dst, Some(inst))
}