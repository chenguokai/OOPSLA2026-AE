package HT.observation.AbstractionLayer

import HT.observation.{SignalReader, ParsedData}
import observation.AbstractionLayer.ObservationAbstractionBase

val XSIBufferOutWidth = 6

val XSIbufferSignalSet: Set[String] = (
  Seq((0 until XSIBufferOutWidth).map{ i =>
    s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_ibuffer.io_out_${i}_valid"
  } ++
  (0 until XSIBufferOutWidth).map{ i =>
    s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_ibuffer.io_out_${i}_bits_pc"
  } ++
  (0 until XSIBufferOutWidth).map{ i =>
    s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_ibuffer.io_out_${i}_bits_foldpc"
  } ++
  (0 until XSIBufferOutWidth).map{ i =>
    s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_ibuffer.io_out_${i}_bits_ftqPtr_flag"
  } ++
  (0 until XSIBufferOutWidth).map{ i =>
    s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_ibuffer.io_out_${i}_bits_ftqPtr_value"
  } ++
  (0 until XSIBufferOutWidth).map{ i =>
    s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_ibuffer.io_out_${i}_bits_ftqOffset"
  } ++
  (0 until XSIBufferOutWidth).map{ i =>
    s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_ibuffer.io_out_${i}_bits_pred_taken"
  }).flatten ++
  Seq("TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_ibuffer.io_decodeCanAccept")
).toSet

class XSIBuffer(wp: ParsedData) extends ObservationAbstractionBase {
  val outValid = (0 until XSIBufferOutWidth).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_ibuffer.io_out_${i}_valid")
  }
  val outPC = (0 until XSIBufferOutWidth).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_ibuffer.io_out_${i}_bits_pc")
  }
  val outFoldPC = (0 until XSIBufferOutWidth).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_ibuffer.io_out_${i}_bits_foldpc")
  }
  val outFtqPtrFlag = (0 until XSIBufferOutWidth).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_ibuffer.io_out_${i}_bits_ftqPtr_flag")
  }
  val outFtqPtrValue = (0 until XSIBufferOutWidth).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_ibuffer.io_out_${i}_bits_ftqPtr_value")
  }
  val outFtqOffset = (0 until XSIBufferOutWidth).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_ibuffer.io_out_${i}_bits_ftqOffset")
  }
  val outPredTaken = (0 until XSIBufferOutWidth).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_ibuffer.io_out_${i}_bits_pred_taken")
  }

  val outCanAccept = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_ibuffer.io_decodeCanAccept")

  def hasValids(cycle: Int): Boolean = {
    outValid.map(_.get_at_time(cycle).asBoolean).reduce(_ || _)
  }
  
  def PCWithinValid(cycle: Int, pc: Long): Int = {
    // the exact index of the valid output that matches the PC
    (0 until XSIBufferOutWidth).find { i =>
      outValid(i).get_at_time(cycle).asBoolean && outPC(i).get_at_time(cycle).toBigInt() == pc
    }.getOrElse(-1)
  }
  
  def getFoldPC(cycle: Int, slot: Long): Long = {
    // return the fold PC for the given slot at the given cycle
    outFoldPC(slot.toInt).get_at_time(cycle).toBigInt().toLong
  }
  
  def getPC(cycle: Int, slot: Int): Long = {
    // return the PC for the given slot at the given cycle
    outPC(slot).get_at_time(cycle).toBigInt().toLong
  }
  
  def getFTQPtr(cycle: Int, slot: Int): (BigInt, BigInt) = {
    (outFtqPtrFlag(slot).get_at_time(cycle).toBigInt(), outFtqPtrValue(slot).get_at_time(cycle).toBigInt())
  }
  
  def getFTQOffset(cycle: Int, slot: Int): Long = {
    // return the FTQ offset for the given slot at the given cycle
    outFtqOffset(slot).get_at_time(cycle).toBigInt().toLong
  }

  def getPredTaken(cycle: Int, slot: Int): Boolean = {
    // return the prediction taken status for the given slot at the given cycle
    outPredTaken(slot).get_at_time(cycle).asBoolean
  }

  def outCanAccept(cycle: Int): Boolean = {
    // check if the buffer can accept new instructions at the given cycle
    outCanAccept.get_at_time(cycle).asBoolean
  }

}