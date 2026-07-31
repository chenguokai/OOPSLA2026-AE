package HT.observation.AbstractionLayer

import HT.observation.{SignalReader, ParsedData}
import observation.AbstractionLayer.ObservationAbstractionBase

val CVA6RedirectSignalSet = Set(
  "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.resolved_branch_i.valid",
  "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.resolved_branch_i.pc",
  "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.resolved_branch_i.target_address",
  "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.resolved_branch_i.is_taken",
  "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.resolved_branch_i.is_mispredict"
)

class CVA6Redirect(wp: ParsedData) extends ObservationAbstractionBase {
  val resolveValid = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.resolved_branch_i.valid")
  val resolvePC = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.resolved_branch_i.pc")
  val resolveTarget = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.resolved_branch_i.target_address")
  val resolveTaken = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.resolved_branch_i.is_taken")
  val resolveMispredict = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.i_frontend.resolved_branch_i.is_mispredict")

  /**
   * Checks if a redirect request occurred in the given cycle.
   * In CVA6, a redirect (frontend flush) occurs when a branch result is valid AND it was a misprediction.
   */
  def hasRedirect(cycle: Int): Boolean = {
    resolveValid.get_at_time(cycle).asBoolean && resolveMispredict.get_at_time(cycle).asBoolean
  }

  /**
   * Returns the PC of the branch instruction being resolved.
   */
  def redirectPC(cycle: Int): BigInt = {
    resolvePC.get_at_time(cycle).toBigInt()
  }

  /**
   * Returns the correct target address where the fetch should redirect to.
   */
  def redirectTargetAddress(cycle: Int): BigInt = {
    resolveTarget.get_at_time(cycle).toBigInt()
  }

  def isTaken(cycle: Int): Boolean = {
    resolveTaken.get_at_time(cycle).asBoolean
  }

  // NOTE: The 'isOlderThan' and 'isYoungerThan' logic from XSFTQ is omitted.
  // CVA6 uses 64-bit Virtual Addresses (PCs) rather than FTQ Circular Queue Pointers
  // for this interface, so the (Flag, Value, Offset) comparison logic is not applicable.
}