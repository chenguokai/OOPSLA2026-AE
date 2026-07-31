package HT.observation.AbstractionLayer

import HT.observation.{SignalReader, ParsedData}
import observation.AbstractionLayer.ObservationAbstractionBase

val CVA6BufferWidth = 2

val CVA6BufferSignalSet: Set[String] = (
  Seq("TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.i_instr_queue.valid") ++
    (0 until CVA6BufferWidth).map { i =>
      s"TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.i_instr_queue.instr_data_in[$i].ex_vaddr"
    } ++
    (0 until CVA6BufferWidth).map { i =>
      s"TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.i_instr_queue.instr_data_in[$i].cf"
    }
  ).toSet

// Companion object to hold Enum constants for cleaner reference
object CVA6CFConstants {
  val NoCF   = 0
  val Branch = 1
  val Jump   = 2
  val JumpR  = 3
  val Return = 4
}

class CVA6IBuffer(wp: ParsedData) extends ObservationAbstractionBase {

  // --- Signal Readers ---
  val validSignal = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.i_instr_queue.valid")

  val outPC = (0 until CVA6BufferWidth).map { i =>
    SignalReader(wp, s"TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.i_instr_queue.instr_data_in[$i].ex_vaddr")
  }

  val outCF = (0 until CVA6BufferWidth).map { i =>
    SignalReader(wp, s"TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.i_instr_queue.instr_data_in[$i].cf")
  }

  // --- Basic Accessors ---

  def isSlotValid(cycle: Int, slot: Int): Boolean = {
    validSignal.get_at_time(cycle).toBigInt().testBit(slot)
  }

  def hasValids(cycle: Int): Boolean = {
    validSignal.get_at_time(cycle).toBigInt() > 0
  }

  def PCWithinValid(cycle: Int, pc: Long): Int = {
    (0 until CVA6BufferWidth).find { i =>
      isSlotValid(cycle, i) && outPC(i).get_at_time(cycle).toBigInt() == pc
    }.getOrElse(-1)
  }

  def getPC(cycle: Int, slot: Int): BigInt = {
    outPC(slot).get_at_time(cycle).toBigInt()
  }

  // --- Control Flow (CF) Interpretation Utilities ---

  /**
   * Returns the raw integer value of the Control Flow signal.
   */
  def getCFValue(cycle: Int, slot: Int): Int = {
    outCF(slot).get_at_time(cycle).toBigInt().toInt
  }

  /**
   * Returns a human-readable string corresponding to the cf_t enum.
   */
  def getCFTypeString(cycle: Int, slot: Int): String = {
    getCFValue(cycle, slot) match {
      case CVA6CFConstants.NoCF   => "NoCF"
      case CVA6CFConstants.Branch => "Branch"
      case CVA6CFConstants.Jump   => "Jump"    // Immediate Jump
      case CVA6CFConstants.JumpR  => "JumpR"   // Register Jump
      case CVA6CFConstants.Return => "Return"
      case other                  => s"Unknown($other)"
    }
  }

  /**
   * Returns true if any control flow prediction exists (Branch, Jump, JumpR, or Return).
   * Equivalent to 'pred_taken' in XiangShan.
   */
  def isControlFlow(cycle: Int, slot: Int): Boolean = {
    getCFValue(cycle, slot) != CVA6CFConstants.NoCF
  }

  /**
   * Specific check for conditional Branches.
   */
  def isBranch(cycle: Int, slot: Int): Boolean = {
    getCFValue(cycle, slot) == CVA6CFConstants.Branch
  }

  /**
   * Specific check for Jumps (Immediate).
   */
  def isJump(cycle: Int, slot: Int): Boolean = {
    getCFValue(cycle, slot) == CVA6CFConstants.Jump
  }

  /**
   * Specific check for Register Jumps (JumpR).
   */
  def isJumpR(cycle: Int, slot: Int): Boolean = {
    getCFValue(cycle, slot) == CVA6CFConstants.JumpR
  }

  /**
   * Specific check for Returns.
   */
  def isReturn(cycle: Int, slot: Int): Boolean = {
    getCFValue(cycle, slot) == CVA6CFConstants.Return
  }

  /**
   * Returns true if the instruction is any kind of Jump (Jump, JumpR, or Return).
   * Often useful to distinguish unconditional control flow from conditional branches.
   */
  def isAnyJump(cycle: Int, slot: Int): Boolean = {
    val cf = getCFValue(cycle, slot)
    cf == CVA6CFConstants.Jump || cf == CVA6CFConstants.JumpR || cf == CVA6CFConstants.Return
  }
}