package HT.observation.AbstractionLayer

import HT.observation.{SignalReader, ParsedData}
import observation.AbstractionLayer.ObservationAbstractionBase

val XSRenameWidth = 6

val XSRenameSignalSet: Set[String] = (
  Seq("TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rename.io_redirect_valid",
  "TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rename.io_redirect_bits_robIdx_flag",
  "TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rename.io_redirect_bits_robIdx_value") ++
  Seq((0 until XSRenameWidth).map { i =>
    s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rename.io_out_${i}_valid"
  } ++
  (0 until XSRenameWidth).map { i =>
    s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rename.io_out_${i}_bits_loadWaitBit"
  } ++
  (0 until XSRenameWidth).map { i =>
    s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rename.io_out_${i}_bits_pc"
  }).flatten
).toSet

class XSRename(wp: ParsedData) extends ObservationAbstractionBase {
  val redirectValid = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rename.io_redirect_valid")
  val redirectRobPtrFlag = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rename.io_redirect_bits_robIdx_flag")
  val redirectRobPtrValue = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rename.io_redirect_bits_robIdx_value")
  val renameOutValid = (0 until XSRenameWidth).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rename.io_out_${i}_valid")
  }
  val renameOutLoadWaitBit = (0 until XSRenameWidth).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rename.io_out_${i}_bits_loadWaitBit")
  }
  val renameOutPC = (0 until XSRenameWidth).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rename.io_out_${i}_bits_pc")
  }

  def renameValid(cycle: Int, slot: Int): Boolean = {
    renameOutValid(slot).get_at_time(cycle).asBoolean
  }

  def renameSlotMatchPC(cycle: Int, slot: Int, pc: BigInt): Boolean = {
    renameValid(cycle, slot) && renameOutPC(slot).get_at_time(cycle).toBigInt() == pc
  }

  def renameMatchPC(cycle: Int, pc: BigInt): Int = {
    (0 until XSRenameWidth).find(slot => renameSlotMatchPC(cycle, slot, pc)).getOrElse(-1)
  }

  def renameWaitBit(cycle: Int, slot: Int): Boolean = {
    renameOutLoadWaitBit(slot).get_at_time(cycle).asBoolean
  }
}
