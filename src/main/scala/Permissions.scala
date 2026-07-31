package HT.Permissions

/*
 * All permissions except AttackerPublicRemote are defined within a single process space
 * ControlPrivate: only accessible from Control world
 * VictimPrivate: only accessible from Victim world
 * AttackerPrivate: only accessible from Attacker world
 * VictimPublic: accessible from both Control and Victim world
 * AttackerPublic: accessible from both Control and Attacker world
 * AttackerPublicRemote variables are defined for a remote process space where it acts like AttackerPublic/AttackerPrivate
 * It can only be read from Control world from local process
 */

enum Permission:
  case ControlPrivate, VictimPrivate, AttackerPrivate, VictimPublic, AttackerPublic, AttackerPrivateRemote, AttackerPublicRemote

var CurrentDefaultPermission = Permission.VictimPrivate

def resetWorldPermission(): Unit = {
  CurrentDefaultPermission = Permission.VictimPrivate
}

def Perm2Bits(perm: Permission): Int = {
  // bit vector
  // bit 0: accessible from Victim
  // bit 1: accessible from Control
  // bit 2: accessible from Attacker
  perm match {
    case Permission.ControlPrivate => 0b010
    case Permission.VictimPrivate => 0b001
    case Permission.AttackerPrivate => 0b100
    case Permission.VictimPublic => 0b011
    case Permission.AttackerPublic => 0b110
  }
}

def Bits2Perm(bits: Int): Permission = {
  bits match {
    case 0b010 => Permission.ControlPrivate
    case 0b001 => Permission.VictimPrivate
    case 0b100 => Permission.AttackerPrivate
    case 0b011 => Permission.VictimPublic
    case 0b110 => Permission.AttackerPublic
    case _ => throw new Exception("Invalid permission bits")
  }
}

def InferPermission(a: Permission, b: Permission): Permission = {
  // infer the permission of the result of a and b
  val a_bits = Perm2Bits(a)
  val b_bits = Perm2Bits(b)
  val result_bits = a_bits & b_bits
  Bits2Perm(result_bits)
}

def PermEqual(a: Permission, b: Permission): Boolean = {
  if (Perm2Bits(a) == Perm2Bits(b)) {
    true
  } else {
    throw new Exception("Permission not equal: " + a + " and " + b)
  }
}

def PermCompatible(dst: Permission, src: Permission): Permission = {
  // check if src is compatible with dst
  val dst_bits = Perm2Bits(dst)
  val src_bits = Perm2Bits(src)
  // result is the common part of dst and src
  if ((dst_bits & src_bits) != 0) {
    val result_bits = dst_bits & src_bits
    Bits2Perm(result_bits)
  } else {
    throw new Exception("Permission not compatible: " + dst + " and " + src)
  }
}

def WorldPermCompatible(src: Permission): Permission = {
  PermCompatible(CurrentDefaultPermission, src)
}