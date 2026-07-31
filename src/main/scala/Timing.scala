package HT

import HT.ASTNodes.{DeclareNode, TimingNode, ValueNode}
import HT.Permissions.CurrentDefaultPermission
import HT.Types.{Timer, types}

object TimingNameManager {
  private val TimingPrefix = "timing_"
  protected var currentTiming = TimingPrefix + "0"
  protected var count = 0
  def incrementCount = {
    currentTiming = TimingPrefix + count.toString
  }
  def currentName = currentTiming
}

def Timing(dst: => ValueNode)(block: => Any) = {
  TimingNameManager.incrementCount
  val orig_sp = GlobalStack.size()
  block
  val statements = GlobalStack.slice(orig_sp, GlobalStack.size())
  GlobalStack.pop(GlobalStack.size() - orig_sp)

  val timingNode = TimingNode(dst, TimingNameManager.currentName, statements.toList)
  GlobalStack.push(timingNode)
}