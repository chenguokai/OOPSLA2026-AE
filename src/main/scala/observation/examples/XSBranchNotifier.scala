package HT.observation.examples

import HT.observation.{SignalReader, WaveformConsumer, ParsedData}

import scala.collection.mutable.ArrayBuffer

val branch_notifier_signals = Set(
    "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_pc_3",
    "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_valid_3",
    "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_offsets_0",
    "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_offsets_1",
    "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_br_taken_mask_0",
    "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_br_taken_mask_1",
    "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_targets_0",
    "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_targets_1",
    "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_slot_valids_0",
    "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_slot_valids_1"
)

class BranchNotifier(wp: ParsedData) extends WaveformConsumer {

    val pred3_pc_follower = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_pc_3")
    val pred3_valid_follower = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_valid_3")
    
    val offset_0_follower = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_offsets_0")
    val offset_1_follower = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_offsets_1")
    val taken_0_follower = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_br_taken_mask_0")
    val taken_1_follower = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_br_taken_mask_1")
    val valid_0_follower = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_slot_valids_0")
    val valid_1_follower = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_slot_valids_1")
    val target_0_follower = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_targets_0")
    val target_1_follower = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.io_bpu_to_ftq_resp_bits_s3_full_pred_3_targets_1")

    class BranchPrediction(val source_pc: Long, val target_pc: Long, val time: Long)

    def parse(): ArrayBuffer[BranchPrediction] = {
        val lt = wp.getMaxTime

        val branch_pcs = ArrayBuffer[BranchPrediction]()

        for cycle_time <- 0L to lt do {
            val pc3_valid = pred3_valid_follower.get_at_time(cycle_time).asBoolean
            
            if pc3_valid then {
                val pc3 = pred3_pc_follower.get_at_time(cycle_time).toBigInt()
                val taken_0 = taken_0_follower.get_at_time(cycle_time).asBoolean
                val valid_0 = valid_0_follower.get_at_time(cycle_time).asBoolean
                val taken_1 = taken_1_follower.get_at_time(cycle_time).asBoolean
                val valid_1 = valid_1_follower.get_at_time(cycle_time).asBoolean

                if taken_0  && valid_0 then {
                    val offset_0 = offset_0_follower.get_at_time(cycle_time).toBigInt()
                    val br_0_pc = pc3 + offset_0 * 2
                    val br_0_trg = target_0_follower.get_at_time(cycle_time).toBigInt().toLong

                    branch_pcs += BranchPrediction(br_0_pc.toLong, br_0_trg, cycle_time)
                } else if taken_1 && valid_1 then {
                    val offset_1 = offset_1_follower.get_at_time(cycle_time).toBigInt()
                    val br_1_pc = pc3 + offset_1 * 2
                    val br_1_trg = target_1_follower.get_at_time(cycle_time).toBigInt().toLong

                    branch_pcs += BranchPrediction(br_1_pc.toLong, br_1_trg, cycle_time)
                }
            }
        }
        branch_pcs
    }  

    def getUsedSignals: Set[String] = {
        branch_notifier_signals
    }
   
}
