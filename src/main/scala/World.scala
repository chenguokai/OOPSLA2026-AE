package HT

import HT.ASTNodes.*
import HT.AttackerZones.{DifferentProcessSwitch, DifferentProcessSMT, SameProcess, SameProcessSMT, SameProcessSwitch, Sequential}
import HT.GlobalStack
import HT.Permissions.*
import HT.Permissions.Permission.*

enum AttackerZones {
  case Sequential           // Sequential execution within current control flow
  case SameProcessSwitch    // Two threads within same process, run on same core, switch by context switch
  case SameProcessSMT     // two threads within same process, run on SMT cores
  case SameProcess        // two threads within same process, run on different cores
  case DifferentProcessSwitch // Two processes run on same on, switch by context switch
  case DifferentProcessSMT // two processes run on SMT cores
  case DifferentProcess    // two processes run on different cores
}

def InSameProcess(zone: AttackerZones): Boolean = {
  zone == Sequential || zone == SameProcessSwitch || zone == SameProcessSMT || zone == SameProcess
}

def OnSameCore(zones: AttackerZones): Boolean = {
  zones == Sequential || zones == SameProcessSwitch || zones == DifferentProcessSwitch
}

def OnSMTCore(zones: AttackerZones): Boolean = {
  zones == SameProcessSMT || zones == DifferentProcessSMT
}

def Victim(block: => Any): WorldNode = {
  //println("Victim block")
  val origPermission = CurrentDefaultPermission
  if (origPermission == AttackerPrivate) {
    // Used a Victim block inside an Attacker block
    throw new Exception("Victim block inside Attacker block")
  }
  if (origPermission == ControlPrivate) {
    // Used a Victim block inside a Control block
    throw new Exception("Victim block inside Control block")
  }
  CurrentDefaultPermission = VictimPrivate
  // recursive just like otherwise
  val orig_sp = GlobalStack.size()
  println("victim sp: " + orig_sp)
  block
  println("victim sp after evaluating block " + GlobalStack.size())
  val statements = GlobalStack.slice(orig_sp, GlobalStack.size())
  println("victim statments " + statements)
  println("victim statement count: " + statements.size)
  GlobalStack.pop(GlobalStack.size() - orig_sp)
  // push onto the stack in WhenNode apply
  val ret = WorldNode("Victim", statements.toList)
  CurrentDefaultPermission = origPermission
  ret
}

def Control()(block: => Any): WorldNode = {
  //println("Control block")
  val origPermission = CurrentDefaultPermission
  if (origPermission == AttackerPrivate) {
    // Used a Control block inside an Attacker block
    throw new Exception("Control block inside Attacker block")
  }
  CurrentDefaultPermission = ControlPrivate
  // recursive just like otherwise
  val orig_sp = GlobalStack.size()
  block
  val statements = GlobalStack.slice(orig_sp, GlobalStack.size())
  println("control statement count: " + statements.size)
  println("pop count " + (GlobalStack.size() - orig_sp))
  GlobalStack.pop(GlobalStack.size() - orig_sp)
  // push onto the stack in WhenNode apply
  val ret = WorldNode("Control", statements.toList)
  CurrentDefaultPermission = origPermission
  ret
}

def Attacker()(block: => Any): WorldNode = {
  val origPermission = CurrentDefaultPermission
  CurrentDefaultPermission = AttackerPrivate
  // recursive just like otherwise
  val orig_sp = GlobalStack.size()
  block
  val statements = GlobalStack.slice(orig_sp, GlobalStack.size())
  GlobalStack.pop(GlobalStack.size() - orig_sp)
  // push onto the stack in WhenNode apply
  val ret = WorldNode("Attacker", statements.toList, None, None)
  CurrentDefaultPermission = origPermission
  ret
}

def Attacker(zone: AttackerZones, pidVar: ValueNode)(block: => Any): WorldNode = {
  //println("Attacker block")
  if (zone == Sequential) {
    throw new Exception("pidVar only valid for Non Sequential zone")
  }
  val origPermission = CurrentDefaultPermission
  CurrentDefaultPermission = AttackerPrivate
  // recursive just like otherwise
  val orig_sp = GlobalStack.size()
  block
  val statements = GlobalStack.slice(orig_sp, GlobalStack.size())
  GlobalStack.pop(GlobalStack.size() - orig_sp)
  // push onto the stack in WhenNode apply
  val ret = WorldNode("Attacker", statements.toList, Some(zone), Some(pidVar))
  CurrentDefaultPermission = origPermission
  ret
}

def ThreadJoin(pidVar: ValueNode) = {
  val node = ThreadJoinNode(pidVar)
  GlobalStack.push(node)
}

def ProcessJoin(pidVar: ValueNode): Unit = {
  val node = ProcessJoinNode(pidVar)
  GlobalStack.push(node)
}