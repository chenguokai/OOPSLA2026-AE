package HT.observation.AbstractionLayer

import HT.observation.{SignalReader, ParsedData}
import observation.AbstractionLayer.ObservationAbstractionBase

val CVA6BPUSignalSet = Set(
  "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.bht_gen.i_bht.bht_update_i.pc",
  "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.bht_gen.i_bht.bht_update_i.taken",
  "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.bht_gen.i_bht.bht_update_i.valid",
  "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.bht_gen.i_bht.update_row_index",
  "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.btb_gen.i_btb.btb_update_i.valid",
  "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.btb_gen.i_btb.btb_update_i.target_address",
  "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.btb_gen.i_btb.btb_update_i.pc",
  "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.btb_gen.i_btb.update_row_index",
  "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.bp_valid",
  "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.is_branch",
  "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.is_call",
  "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.is_jalr",
  "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.is_jump",
  "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.predict_address",
  "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.icache_vaddr_q" // Added current PC signal
)

class CVA6BPU(wp: ParsedData) extends ObservationAbstractionBase {
  // ===========================================================================
  // Update Signals (BHT & BTB)
  // ===========================================================================

  val bhtUpdatePc = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.bht_gen.i_bht.bht_update_i.pc")
  val bhtUpdateTaken = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.bht_gen.i_bht.bht_update_i.taken")
  val bhtUpdateValid = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.bht_gen.i_bht.bht_update_i.valid")

  val btbUpdateValid = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.btb_gen.i_btb.btb_update_i.valid")
  val btbUpdateTarget = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.btb_gen.i_btb.btb_update_i.target_address")
  val btbUpdatePc = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.btb_gen.i_btb.btb_update_i.pc")

  // ===========================================================================
  // Prediction Signals (Frontend Output)
  // ===========================================================================

  val predValid = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.bp_valid")
  val predTarget = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.predict_address")
  val fetchPc = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.icache_vaddr_q") // Added reader

  val predIsBranch = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.is_branch")
  val predIsCall = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.is_call")
  val predIsJalr = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.is_jalr")
  val predIsJump = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.is_jump")

  // ===========================================================================
  // Update Logic Implementation
  // ===========================================================================

  def updateValid(cycle: Int): Boolean = {
    bhtUpdateValid.get_at_time(cycle).asBoolean || btbUpdateValid.get_at_time(cycle).asBoolean
  }

  def updatePc(cycle: Int): BigInt = {
    if (bhtUpdateValid.get_at_time(cycle).asBoolean) {
      bhtUpdatePc.get_at_time(cycle).toBigInt()
    } else {
      btbUpdatePc.get_at_time(cycle).toBigInt()
    }
  }

  def updateTaken(cycle: Int): Boolean = {
    if (bhtUpdateValid.get_at_time(cycle).asBoolean) {
      bhtUpdateTaken.get_at_time(cycle).asBoolean
    } else {
      true
    }
  }

  def updateTargets(cycle: Int): List[Option[BigInt]] = {
    val hasTarget = btbUpdateValid.get_at_time(cycle).asBoolean
    val target = if (hasTarget) {
      Some(btbUpdateTarget.get_at_time(cycle).toBigInt())
    } else {
      None
    }
    List(target)
  }

  def updateBlockEnd(cycle: Int): BigInt = {
    if (updateTaken(cycle)) {
      updateTargets(cycle).head.getOrElse(updatePc(cycle))
    } else {
      updatePc(cycle) + 4
    }
  }

  // ===========================================================================
  // Prediction Logic Implementation
  // ===========================================================================

  def lastOutValid(cycle: Int): Boolean = {
    predValid.get_at_time(cycle).asBoolean
  }

  def lastOutTarget(cycle: Int): BigInt = {
    predTarget.get_at_time(cycle).toBigInt()
  }

  def lastOutPc(cycle: Int): BigInt = {
    // Corrected to use icache_vaddr_q
    fetchPc.get_at_time(cycle).toBigInt()
  }

  def lastOutTaken(cycle: Int): Boolean = {
    predValid.get_at_time(cycle).asBoolean
  }

  def getTypeMask(cycle: Int, reader: SignalReader): Int = {
    reader.get_at_time(cycle).toBigInt().toInt
  }

  def isBranch(cycle: Int): Int = getTypeMask(cycle, predIsBranch)
  def isCall(cycle: Int): Int = getTypeMask(cycle, predIsCall)
  def isJalr(cycle: Int): Int = getTypeMask(cycle, predIsJalr)
  def isJump(cycle: Int): Int = getTypeMask(cycle, predIsJump)
}