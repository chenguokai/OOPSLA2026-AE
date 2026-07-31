package HT.observation.AbstractionLayer

import HT.observation.{SignalReader, ParsedData}
import observation.AbstractionLayer.ObservationAbstractionBase

val XSTAGETableCount = 4

val XSTAGESignalSet: Set[String] = (
  Seq((0 until XSTAGETableCount).map { i =>
    s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.predictors.tage.tables_${i}.io_update_pc"
  } ++
    (0 until XSTAGETableCount).map { i =>
      s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.predictors.tage.tables_${i}.io_update_ghist"
    } ++
    (0 until XSTAGETableCount).map { i =>
      s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.predictors.tage.tables_${i}.io_update_mask_0"
    } ++
    (0 until XSTAGETableCount).map { i =>
      s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.predictors.tage.tables_${i}.io_update_mask_1"
    } ++
    (0 until XSTAGETableCount).map { i =>
      s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.predictors.tage.tables_${i}.update_tag"
    } ++
    (0 until XSTAGETableCount).map { i =>
      s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.predictors.tage.tables_${i}.update_idx"
    }
  ).flatten
  ).toSet


class XSTAGE(wp: ParsedData) extends ObservationAbstractionBase {
  val updatePC = (0 until XSTAGETableCount).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.predictors.tage.tables_${i}.io_update_pc")
  }
  val updateGHist = (0 until XSTAGETableCount).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.predictors.tage.tables_${i}.io_update_ghist")
  }
  val updateMask0 = (0 until XSTAGETableCount).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.predictors.tage.tables_${i}.io_update_mask_0")
  }
  val updateMask1 = (0 until XSTAGETableCount).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.predictors.tage.tables_${i}.io_update_mask_1")
  }
  val updateTag = (0 until XSTAGETableCount).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.predictors.tage.tables_${i}.update_tag")
  }
  val updateIdx = (0 until XSTAGETableCount).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_bpu.predictors.tage.tables_${i}.update_idx")
  }
  
  def updateValid(cycle: Int, table: Int): Boolean = {
    updateMask0(table).get_at_time(cycle).asBoolean || updateMask1(table).get_at_time(cycle).asBoolean
  }
  
  def getUpdatePC(cycle: Int, table: Int): BigInt = {
    updatePC(table).get_at_time(cycle).toBigInt()
  }
  
  def getUpdateGHist(cycle: Int, table: Int): BigInt = {
    updateGHist(table).get_at_time(cycle).toBigInt()
  }
  
  def getUpdateMask0(cycle: Int, table: Int): BigInt = {
    updateMask0(table).get_at_time(cycle).toBigInt()
  }
  
  def getUpdateMask1(cycle: Int, table: Int): BigInt = {
    updateMask1(table).get_at_time(cycle).toBigInt()
  }
  
  def getUpdateTag(cycle: Int, table: Int): BigInt = {
    updateTag(table).get_at_time(cycle).toBigInt()
  }
  
  def getUpdateIdx(cycle: Int, table: Int): BigInt = {
    updateIdx(table).get_at_time(cycle).toBigInt()
  }
  
  def updateValidPos(cycle: Int): Int = {
    // at most one table can be valid at a time
    (0 until XSTAGETableCount).find { i =>
      updateValid(cycle, i)
    }.getOrElse(-1)
  }

}