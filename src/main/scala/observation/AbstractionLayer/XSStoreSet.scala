package HT.observation.AbstractionLayer

import HT.observation.{SignalReader, ParsedData}
import observation.AbstractionLayer.ObservationAbstractionBase

val XSStoreSetSignalSet = Set(
  "TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.memCtrl.ssit.io_update_valid",
  "TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.memCtrl.ssit.io_update_ldpc",
  "TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.memCtrl.ssit.io_update_stpc",
)
class XSStoreSet(wp: ParsedData) extends ObservationAbstractionBase {
  val updateValid = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.memCtrl.ssit.io_update_valid")
  val updateLdPC = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.memCtrl.ssit.io_update_ldpc")
  val updateStPC = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.memCtrl.ssit.io_update_stpc")
  
  def hasUpdate(cycle: Int): Boolean = {
    updateValid.get_at_time(cycle).asBoolean
  }
  
  def updateLdPC(cycle: Int): BigInt = {
    updateLdPC.get_at_time(cycle).toBigInt()
  }
  def updateStPC(cycle: Int): BigInt = {
    updateStPC.get_at_time(cycle).toBigInt()
  }
  
  def updateMatches(cycle: Int, ldPC: BigInt, stPC: BigInt): Boolean = {
    hasUpdate(cycle) && updateLdPC(cycle) == ldPC && updateStPC(cycle) == stPC
  }
}
