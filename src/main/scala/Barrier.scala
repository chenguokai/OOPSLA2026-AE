package HT

import HT.ASTNodes._

def Mfence(inst: Option[ObjectValueNode] = None): Unit = {
  val (padding, uniname) = PrepareInstHandle(inst)
  val mfence = MFenceNode(inst, padding, uniname)
  GlobalStack.push(mfence)
}

