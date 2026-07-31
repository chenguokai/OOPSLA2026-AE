package HT.observation.AbstractionLayer

import HT.observation.{SignalReader, ParsedData}
import observation.AbstractionLayer.ObservationAbstractionBase

val XSBPUSignalSet = Set(
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_valid",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_pc",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_ftb_entry_valid",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_ftb_entry_brSlots_0_valid",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_ftb_entry_tailSlot_valid",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_ftb_entry_brSlots_0_offset",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_ftb_entry_tailSlot_offset",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_ftb_entry_brSlots_0_lower",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_ftb_entry_tailSlot_lower",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_ftb_entry_brSlots_0_tarStat",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_ftb_entry_tailSlot_tarStat",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_valid_0",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_pc_0",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_hit",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_slot_valids_0",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_slot_valids_1",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_br_taken_mask_0",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_br_taken_mask_1",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_offsets_0",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_offsets_1",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_targets_0",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_targets_1",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_fallThroughAddr",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_br_taken_mask_0",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_br_taken_mask_1",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_cfi_idx_bits",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_cfi_idx_valid",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_pc",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_mispred_mask_0",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_mispred_mask_1",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_mispred_mask_2", // last is for jmpOffset, we normally do not care
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_ftb_entry_isCall",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_ftb_entry_isRet",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_ftb_entry_isJalr",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_ftq_idx_flag",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_ftq_idx_value",
)

class XSBPU(wp: ParsedData) extends ObservationAbstractionBase {
  // for BPU update signals
  val updateValid = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_valid")
  val updatePc = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_pc")
  val updateFTBEntryValid = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_ftb_entry_valid")
  val updateFTBEntrySlot0Valid = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_ftb_entry_brSlots_0_valid")
  val updateFTBEntrySlot1Valid = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_ftb_entry_tailSlot_valid")
  val updateFTBEntrySlot0Offset = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_ftb_entry_brSlots_0_offset")
  val updateFTBEntrySlot1Offset = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_ftb_entry_tailSlot_offset")
  val updateFTBEntrySlot0TargetLower = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_ftb_entry_brSlots_0_lower")
  val updateFTBEntrySlot1TargetLower = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_ftb_entry_tailSlot_lower")
  val updateFTBEntrySlot0Tarstat = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_ftb_entry_brSlots_0_tarStat")
  val updateFTBEntrySlot1Tarstat = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_ftb_entry_tailSlot_tarStat")
  val updateBrTaken0 = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_br_taken_mask_0")
  val updateBrTaken1 = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_br_taken_mask_1")
  val updateCfiOffset = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_cfi_idx_bits")
  val updateCfiValid = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_cfi_idx_valid")

  val updatePC = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_pc")
  val updateMispredMask0 = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_mispred_mask_0")
  val updateMispredMask1 = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_mispred_mask_1")
  val updateMispredMask2 = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_mispred_mask_2") // last is for jmpOffset, we normally do not care
  val updateIsCall = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_ftb_entry_isCall")
  val updateIsRet = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_ftb_entry_isRet")
  val updateIsJalr = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_ftq_to_bpu_update_bits_ftb_entry_isJalr")

  def tarFit = 0
  def tarOVF = 1
  def tarUDF = 2

  def SLOT0_LOWER = 12 // the highest bit of the lower part of the target address
  def SLOT1_LOWER = 20



  // for BPU prediction signals
  val pred3Valid = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_valid_0")
  val pred3Pc = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_pc_0")
  val pred3FullPredValid = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_hit")
  val pred3FullPredSlot0Valid = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_slot_valids_0")
  val pred3FullPredSlot1Valid = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_slot_valids_1")
  val pred3FullPredSlot0Taken = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_br_taken_mask_0")
  val pred3FullPredSlot1Taken = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_br_taken_mask_1")
  val pred3FullPredSlot0Offset = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_offsets_0")
  val pred3FullPredSlot1Offset = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_offsets_1")
  val pred3FullPredSlot0Target = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_targets_0")
  val pred3FullPredSlot1Target = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_targets_1")
  val pred3FullPredFallthrough = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_fallThroughAddr")
  val pred3FtqFlag = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_ftq_idx_flag")
  val pred3FtqValue = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_ftq_idx_value")

  def updateValid(cycle: Int): Boolean = {
    updateValid.get_at_time(cycle).asBoolean
  }

  def updateBlockEnd(cycle: Int): BigInt = {
    // we would expect that the validity has been checked by the user
    val slot0Valid = updateFTBEntrySlot0Valid.get_at_time(cycle).asBoolean
    val slot1Valid = updateFTBEntrySlot1Valid.get_at_time(cycle).asBoolean
    val slot0Offset = updateFTBEntrySlot0Offset.get_at_time(cycle).toBigInt()
    val slot1Offset = updateFTBEntrySlot1Offset.get_at_time(cycle).toBigInt()

    if (slot1Valid) {
      updatePc(cycle) + slot1Offset * 2
    } else if (slot0Valid) {
      updatePc(cycle) + slot0Offset * 2
    } else {
      + 32 // default fall-through address offset
    }
  }

  def updatePc(cycle: Int): BigInt = {
    // we would expect that the validity has been checked by the user
    updatePc.get_at_time(cycle).toBigInt()
  }

  def withinUpdateRange(cycle: Int, pc: BigInt): Boolean = {
    // we would expect that the validity has been checked by the user
    pc >= updatePc(cycle) && pc <= updateBlockEnd(cycle)
  }

  def updateTargets(cycle: Int): List[Option[BigInt]] = {
    // we would expect that the validity has been checked by the user
    val slot0Valid = updateFTBEntrySlot0Valid.get_at_time(cycle).asBoolean
    val slot1Valid = updateFTBEntrySlot1Valid.get_at_time(cycle).asBoolean
    val slot0Offset = updateFTBEntrySlot0Offset.get_at_time(cycle).toBigInt()
    val slot1Offset = updateFTBEntrySlot1Offset.get_at_time(cycle).toBigInt()
    val slot0TargetLower = updateFTBEntrySlot0TargetLower.get_at_time(cycle).toBigInt()
    val slot1TargetLower = updateFTBEntrySlot1TargetLower.get_at_time(cycle).toBigInt()
    val slot0TarStat = updateFTBEntrySlot0Tarstat.get_at_time(cycle).toBigInt()
    val slot1TarStat = updateFTBEntrySlot1Tarstat.get_at_time(cycle).toBigInt()
    val pc = updatePc(cycle)

    // full target address generated by lower, pc and tarStat
    // lower + pc (lower part masked) + (determined by tarStat) (1/-1/0) << (highest bit of lower + 1)
    val slot0Target = if (slot0Valid) {
      Some(pc + slot0TargetLower + ((if (slot0TarStat == tarFit) 1 else if (slot0TarStat == tarOVF) -1 else 0) << SLOT0_LOWER))
    } else {
      None
    }

    val slot1Target = if (slot1Valid) {
      // println(f"debug targetLower: $slot1TargetLower%X, tarStat: $slot1TarStat%X, pc: $pc%X")
      Some(pc + (slot1TargetLower << 1) + ((if (slot1TarStat == tarUDF) -1 else if (slot1TarStat == tarOVF) 1 else 0) << SLOT1_LOWER))
    } else {
      None
    }

    List(slot0Target, slot1Target)
  }

  def updateOffsets(cycle: Int): List[Option[BigInt]] = {
    // we would expect that the validity has been checked by the user
    val slot0Valid = updateFTBEntrySlot0Valid.get_at_time(cycle).asBoolean
    val slot1Valid = updateFTBEntrySlot1Valid.get_at_time(cycle).asBoolean
    val slot0Offset = updateFTBEntrySlot0Offset.get_at_time(cycle).toBigInt()
    val slot1Offset = updateFTBEntrySlot1Offset.get_at_time(cycle).toBigInt()

    List(if (slot0Valid) Some(slot0Offset) else None, if (slot1Valid) Some(slot1Offset) else None)
  }

  def hasMispred(cycle: Int): Boolean = {
    updateValid.get_at_time(cycle).asBoolean && (updateMispredMask0.get_at_time(cycle).asBoolean || updateMispredMask1.get_at_time(cycle).asBoolean)
  }
  
  def MispredPC(cycle: Int): BigInt = {
    if (updateMispredMask0.get_at_time(cycle).asBoolean) {
      // mispred in slot 0
      val slot0Offset = updateFTBEntrySlot0Offset.get_at_time(cycle).toBigInt()
      println(f"debug mispred slot0Offset: $slot0Offset, pc: ${updatePc(cycle)}%X, total target: ${updatePc(cycle) + slot0Offset * 2}%X")
      updatePc(cycle) + slot0Offset * 2
    } else if (updateMispredMask1.get_at_time(cycle).asBoolean) {
      // mispred in slot 1
      val slot1Offset = updateFTBEntrySlot1Offset.get_at_time(cycle).toBigInt()
      println(f"debug mispred slot1Offset: $slot1Offset, pc: ${updatePc(cycle)}%X, total target: ${updatePc(cycle) + slot1Offset * 2}%X")
      updatePc(cycle) + slot1Offset * 2
    } else {
      throw new Exception("No mispred detected")
    }
  }

  def updateWithinRange(cycle: Int, pc: BigInt): Boolean = {
    // we would expect that the validity has been checked by the user
    pc >= updatePc(cycle) && pc <= updateBlockEnd(cycle)
  }

  def updateTaken(cycle: Int): Boolean = {
    // here we assume that the update is valid
    updateCfiValid.get_at_time(cycle).asBoolean
  }

  def updateTakenPC(cycle: Int): BigInt = {
    // here we assume that the update is valid
    updatePc(cycle) + updateCfiOffset.get_at_time(cycle).toBigInt() * 2
  }

  def lastOutValid(cycle: Int): Boolean = {
    // last stage prediction valid
    pred3Valid.get_at_time(cycle).asBoolean
  }

  def lastOutTarget(cycle: Int): BigInt = {
    // last stage prediction content
    // if we have got a valid slot 0, target from slot 0,
    // else if we have got a valid slot 1, target from slot 1,
    // else fallThrough address
    val slot0Valid = pred3FullPredSlot0Valid.get_at_time(cycle).asBoolean
    val slot1Valid = pred3FullPredSlot1Valid.get_at_time(cycle).asBoolean
    val slot0Taken = pred3FullPredSlot0Taken.get_at_time(cycle).asBoolean
    val slot1Taken = pred3FullPredSlot1Taken.get_at_time(cycle).asBoolean
    val slot0Target = pred3FullPredSlot0Target.get_at_time(cycle).toBigInt()
    val slot1Target = pred3FullPredSlot1Target.get_at_time(cycle).toBigInt()
    val fallThrough = pred3FullPredFallthrough.get_at_time(cycle).toBigInt()
    // println(f"slot0Valid: $slot0Valid, slot1Valid: $slot1Valid, slot0Taken: $slot0Taken, slot1Taken: $slot1Taken, slot0Target: $slot0Target%X, slot1Target: $slot1Target%X, fallThrough: $fallThrough%X")
    if (slot0Valid && slot0Taken) {
      slot0Target
    } else if (slot1Valid && slot1Taken) {
      slot1Target
    } else {
      fallThrough
    }
  }

  def lastOutPc(cycle: Int): BigInt = {
    // last stage prediction PC
    pred3Pc.get_at_time(cycle).toBigInt()
  }

  def lastOutBlockEnd(cycle: Int): BigInt = {
    // last stage prediction block end
    val slot0Valid = pred3FullPredValid.get_at_time(cycle).asBoolean && pred3FullPredSlot0Valid.get_at_time(cycle).asBoolean
    val slot1Valid = pred3FullPredSlot1Taken.get_at_time(cycle).asBoolean && pred3FullPredSlot1Valid.get_at_time(cycle).asBoolean
    val slot0Offset = pred3FullPredSlot0Offset.get_at_time(cycle).toBigInt()
    val slot1Offset = pred3FullPredSlot1Offset.get_at_time(cycle).toBigInt()

    if (slot1Valid) {
      lastOutPc(cycle) + slot1Offset * 2
    } else if (slot0Valid) {
      lastOutPc(cycle) + slot0Offset * 2
    } else {
      lastOutPc(cycle) + 32 // default fall-through address offset
    }
  }

  def lastOutWithinRange(cycle: Int, pc: BigInt): Boolean = {
    // we would expect that the validity has been checked by the user
    // println(f"debug lastOutPc ${lastOutPc(cycle)}%X lastOutBlockEnd: ${lastOutBlockEnd(cycle)}%X")
    pc >= lastOutPc(cycle) && pc <= lastOutBlockEnd(cycle)
  }

  def lastOutTaken(cycle: Int): Boolean = {
    // last stage prediction taken
    (pred3FullPredSlot0Valid.get_at_time(cycle).asBoolean && pred3FullPredSlot0Taken.get_at_time(cycle).asBoolean) ||
      (pred3FullPredSlot1Valid.get_at_time(cycle).asBoolean && pred3FullPredSlot1Taken.get_at_time(cycle).asBoolean)
  }

  def lastOutFtqFlag(cycle: Int): BigInt = {
    // last stage prediction ftq flag
    pred3FtqFlag.get_at_time(cycle).toBigInt()
  }

  def lastOutFtqValue(cycle: Int): BigInt = {
    // last stage prediction ftq value
    pred3FtqValue.get_at_time(cycle).toBigInt()
  }

  def lastOutFtqPtr(cycle: Int): (BigInt, BigInt) = {
    // last stage prediction ftq pointer
    val ftqFlag = lastOutFtqFlag(cycle)
    val ftqValue = lastOutFtqValue(cycle)
    (ftqFlag, ftqValue)
  }
}
