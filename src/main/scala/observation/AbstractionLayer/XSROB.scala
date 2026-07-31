package HT.observation.AbstractionLayer

import HT.RISCV64.rvRegToInt
import HT.observation.{ParsedData, SignalReader}
import observation.AbstractionLayer.ObservationAbstractionBase

val XSROBCommitSlot = 8
val RVRegCount = 32

val XSROBSignalSet: Set[String] =
  Seq((0 until XSROBCommitSlot).map{
    i => s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rob.io_commits_commitValid_$i"
  } ++
  (0 until XSROBCommitSlot).map{
    i => s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rob.io_commits_robIdx_${i}_flag"
  } ++
  (0 until XSROBCommitSlot).map{
    i => s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rob.io_commits_robIdx_${i}_value"
  } ++
  (0 until XSROBCommitSlot).map{
    i => s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rob.io_commits_info_${i}_ftqIdx_flag"
  } ++
  (0 until XSROBCommitSlot).map{
    i => s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rob.io_commits_info_${i}_ftqIdx_value"
  } ++
  (0 until XSROBCommitSlot).map{
    i => s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rob.io_commits_info_${i}_ftqOffset"
  } ++
    (1 until XSROBCommitSlot).map{
      i => s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rob.difftest_module_${i}.io_valid"
    } ++
    Seq(s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rob.difftest_module.io_valid") ++
    (1 until XSROBCommitSlot).map{
      i => s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rob.difftest_module_${i}.io_bits_pc"
    } ++
    Seq(s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rob.difftest_module.io_bits_pc") ++
    (0 until RVRegCount).map{
      i => s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_dataPath.difftestArchIntRegState_module.io_bits_value_${i}"
    } ++
  (0 until XSROBCommitSlot).map{
    i => s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rob.io_commits_info_${i}_debug_pc"
  }).flatten.toSet


class XSROB(wp: ParsedData) extends ObservationAbstractionBase {
  val robCommitValid = (0 until XSROBCommitSlot).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rob.io_commits_commitValid_$i")
  }
  val robCommitRobIdxFlag = (0 until XSROBCommitSlot).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rob.io_commits_robIdx_${i}_flag")
  }
  val robCommitRobIdxValue = (0 until XSROBCommitSlot).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rob.io_commits_robIdx_${i}_value")
  }
  val robCommitFtqIdxFlag = (0 until XSROBCommitSlot).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rob.io_commits_info_${i}_ftqIdx_flag")
  }
  val robCommitFtqIdxValue = (0 until XSROBCommitSlot).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rob.io_commits_info_${i}_ftqIdx_value")
  }
  val robCommitFtqOffset = (0 until XSROBCommitSlot).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rob.io_commits_info_${i}_ftqOffset")
  }
  val robCommitPC = (0 until XSROBCommitSlot).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rob.io_commits_info_${i}_debug_pc")
  }

  val diffTestCommitValid = Seq(SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rob.difftest_module.io_valid")) ++ (1 until XSROBCommitSlot).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rob.difftest_module_${i}.io_valid")
  }
  val diffTestCommitPC = Seq(SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rob.difftest_module.io_bits_pc")) ++ (1 until XSROBCommitSlot).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_ctrlBlock.rob.difftest_module_${i}.io_bits_pc")
  }
  val diffTestArchIntRegState = (0 until RVRegCount).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.backend.inner_dataPath.difftestArchIntRegState_module.io_bits_value_${i}")
  }

  def getIntReg(cycle: Int, reg: String): Long = {
    diffTestArchIntRegState(rvRegToInt(reg)).get_at_time(cycle).toBigInt().toLong
  }

  def commitWithPC(cycle: Int, pc: Long): Boolean = {
    (0 until XSROBCommitSlot).exists { i =>
      diffTestCommitValid(i).get_at_time(cycle).asBoolean &&
      diffTestCommitPC(i).get_at_time(cycle).toBigInt() == pc
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

  def commitFtqIdxFlag(cycle: Int, slot: Int): BigInt = {
    robCommitFtqIdxFlag(slot).get_at_time(cycle).toBigInt()
  }

  def commitFtqIdxValue(cycle: Int, slot: Int): BigInt = {
    robCommitFtqIdxValue(slot).get_at_time(cycle).toBigInt()
  }

  def commitFtqOffset(cycle: Int, slot: Int): BigInt = {
    // Assuming the offset is stored in a similar way as the flags and values
    robCommitFtqOffset(slot).get_at_time(cycle).toBigInt()
  }

  def getValidFtqIdxes(cycle: Int): List[(BigInt, BigInt)] = {
    (0 until XSROBCommitSlot).flatMap { i =>
      if (commitValid(cycle, i)) {
        // assuming these depend on i; if not, you can omit i
        val flag  = commitFtqIdxFlag(cycle, i)
        val value = commitFtqIdxValue(cycle, i)
        List((flag, value))
      } else {
        Nil
      }
    }.toList
  }

  def getValidFtqs(cycle: Int): List[(BigInt, BigInt, BigInt)] = {
    (0 until XSROBCommitSlot).flatMap { i =>
      if (commitValid(cycle, i)) {
        // assuming these depend on i; if not, you can omit i
        val flag  = commitFtqIdxFlag(cycle, i)
        val value = commitFtqIdxValue(cycle, i)
        val offset = commitFtqOffset(cycle, i)
        List((flag, value, offset))
      } else {
        Nil
      }
    }.toList
  }

  def commitHasFtqIdx(cycle: Int, flag: BigInt, value: BigInt): Boolean = {
    val validFtqIdxes = getValidFtqIdxes(cycle)
    // println(s"Valid FTQ Indexes at cycle $cycle: $validFtqIdxes")
    validFtqIdxes.exists { case (f, v) => f == flag && v == value }
  }

  def commitHasFtq(cycle: Int, flag: BigInt, value: BigInt, offset: BigInt): Boolean = {
    val validFtqIdxes = getValidFtqs(cycle)
    // println(s"Valid FTQ Indexes at cycle $cycle: $validFtqIdxes")
    validFtqIdxes.exists { case (f, v, o) => f == flag && v == value && o == offset }
  }
  
  def commitIsYounger(cycle: Int, flag: BigInt, value: BigInt, offset: BigInt) = {
    // currently committed instructions are younger than the given ftq ptr
    val validFtqIdxes = getValidFtqs(cycle)
    validFtqIdxes.exists { case (f, v, o) =>
      (f != flag && v < value) || (f == flag && v == value && o > offset) || (f == flag && v > value)
    }
  }

  def getPC(cycle: Int, slot: Int): BigInt = {
    robCommitPC(slot).get_at_time(cycle).toBigInt()
  }

  def getValidPCs(cycle: Int): List[BigInt] = {
    (0 until XSROBCommitSlot).flatMap { i =>
      if (commitValid(cycle, i)) {
        // assuming these depend on i; if not, you can omit i
        List(getPC(cycle, i))
      } else {
        Nil
      }
    }.toList
  }
}