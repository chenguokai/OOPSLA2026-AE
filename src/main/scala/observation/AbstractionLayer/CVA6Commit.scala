package HT.observation.AbstractionLayer

import HT.RISCV64.rvRegToInt
import HT.observation.{ParsedData, SignalReader}
import observation.AbstractionLayer.ObservationAbstractionBase

// CVA6 (Ariane) typically has a commit width of 2
val CVA6CommitSlot = 2
val CVA6RegCount = 32

val CVA6CommitSignalSet: Set[String] =
  Seq((0 until CVA6CommitSlot).map{
    i => s"TOP.ariane_testharness.i_ariane.i_cva6.commit_stage_i.commit_instr_i[$i].valid"
  } ++
    (0 until CVA6CommitSlot).map{
      i => s"TOP.ariane_testharness.i_ariane.i_cva6.commit_stage_i.commit_instr_i[$i].pc"
    } ++
    (0 until CVA6CommitSlot).map{
      i => s"TOP.ariane_testharness.i_ariane.i_cva6.commit_stage_i.commit_instr_i[$i].is_compressed"
    } ++
    (0 until CVA6CommitSlot).map{
      i => s"TOP.ariane_testharness.i_ariane.i_cva6.commit_stage_i.commit_instr_i[$i].result"
    } ++
    (0 until CVA6RegCount).map{
      i => s"TOP.ariane_testharness.i_ariane.i_cva6.issue_stage_i.i_issue_read_operands.gen_asic_regfile.i_ariane_regfile.mem[$i]"
    }).flatten.toSet


class CVA6Commit(wp: ParsedData) extends ObservationAbstractionBase {

  // --- Signal Readers ---

  val robCommitValid = (0 until CVA6CommitSlot).map { i =>
    SignalReader(wp, s"TOP.ariane_testharness.i_ariane.i_cva6.commit_stage_i.commit_instr_i[$i].valid")
  }

  val robCommitPC = (0 until CVA6CommitSlot).map { i =>
    SignalReader(wp, s"TOP.ariane_testharness.i_ariane.i_cva6.commit_stage_i.commit_instr_i[$i].pc")
  }

  val robCommitIsCompressed = (0 until CVA6CommitSlot).map { i =>
    SignalReader(wp, s"TOP.ariane_testharness.i_ariane.i_cva6.commit_stage_i.commit_instr_i[$i].is_compressed")
  }

  val robCommitResult = (0 until CVA6CommitSlot).map { i =>
    SignalReader(wp, s"TOP.ariane_testharness.i_ariane.i_cva6.commit_stage_i.commit_instr_i[$i].result")
  }

  // Register File Reader
  // Note: Based on provided signals, mapping directly to the register memory array
  val archIntRegState = (0 until CVA6RegCount).map { i =>
    SignalReader(wp, s"TOP.ariane_testharness.i_ariane.i_cva6.issue_stage_i.i_issue_read_operands.gen_asic_regfile.i_ariane_regfile.mem[$i]")
  }

  // --- Abstraction Methods ---

  def getDiffTestIntReg(cycle: Int, reg: Int): Long = {
    archIntRegState(reg).get_at_time(cycle).toBigInt().toLong
  }

  // CVA6 does not provide a separate 'difftest' module in the signal list,
  // so we reuse the commit stage signals for verification.
  def diffTestCommitWithPC(cycle: Int, pc: Long): Boolean = {
    (0 until CVA6CommitSlot).exists { i =>
      robCommitValid(i).get_at_time(cycle).asBoolean &&
        robCommitPC(i).get_at_time(cycle).toBigInt() == pc
    }
  }

  def commitHasValid(cycle: Int): Boolean = {
    robCommitValid.map{
      _.get_at_time(cycle).asBoolean
    }.reduce(_ || _)
  }

  def commitValid(cycle: Int, slot: Int): Boolean = {
    robCommitValid(slot).get_at_time(cycle).asBoolean
  }

  def getPC(cycle: Int, slot: Int): BigInt = {
    robCommitPC(slot).get_at_time(cycle).toBigInt()
  }

  def getValidPCs(cycle: Int): List[BigInt] = {
    (0 until CVA6CommitSlot).flatMap { i =>
      if (commitValid(cycle, i)) {
        List(getPC(cycle, i))
      } else {
        Nil
      }
    }.toList
  }

  def getCommitResult(cycle: Int, slot: Int): BigInt = {
    robCommitResult(slot).get_at_time(cycle).toBigInt()
  }

  def isCompressed(cycle: Int, slot: Int): Boolean = {
    robCommitIsCompressed(slot).get_at_time(cycle).asBoolean
  }
}