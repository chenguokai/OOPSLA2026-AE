package HT.observation.AbstractionLayer

import HT.observation.{SignalReader, ParsedData}
import observation.AbstractionLayer.ObservationAbstractionBase

val XSDCacheLoadPort = 3 // number of load ports in the cache

val XSDCacheSignalSet: Set[String] = (
  Seq((0 until XSDCacheLoadPort).map { i =>
    s"TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.io_lsu_load_${i}_req_valid"
  } ++
  (0 until XSDCacheLoadPort).map { i =>
    s"TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.io_lsu_load_${i}_req_ready"
  } ++
  (0 until XSDCacheLoadPort).map { i =>
    s"TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.io_lsu_load_${i}_req_bits_vaddr"
  } ++
    (0 until XSDCacheLoadPort).map { i =>
      s"TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.io_lsu_load_${i}_req_bits_cmd" // for testing only
    }).flatten ++
  Seq(
    "TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.dcache.mainPipe.io_refill_req_valid",
    "TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.dcache.mainPipe.io_refill_req_ready",
    "TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.dcache.mainPipe.io_refill_req_bits_addr",
    "TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.dcache.missQueue.io_req_valid",
    "TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.dcache.missQueue.io_req_ready",
    "TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.dcache.missQueue.io_req_bits_vaddr",
    "TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.dcache.missQueue.io_req_bits_addr",
    "TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.dcache.mainPipe.s2_valid",
    "TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.dcache.mainPipe.s2_req_vaddr",
    "TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.dcache.mainPipe.s2_ready",
    "TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.dcache.mainPipe.s2_need_eviction"
  )).toSet


class XSDCache(wp: ParsedData) extends ObservationAbstractionBase {
  val loadReqValid = (0 until XSDCacheLoadPort).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.io_lsu_load_${i}_req_valid")
  }
  val loadReqReady = (0 until XSDCacheLoadPort).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.io_lsu_load_${i}_req_ready")
  }
  val loadReqBitsVaddr = (0 until XSDCacheLoadPort).map { i =>
    SignalReader(wp, s"TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.io_lsu_load_${i}_req_bits_vaddr")
  }
  val mainPipeMissReqValid = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.dcache.missQueue.io_req_valid")
  val mainPipeMissReqReady = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.dcache.missQueue.io_req_ready")
  val mainPipeMissReqBitsVaddr = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.dcache.missQueue.io_req_bits_vaddr")
  val mainPipeMissReqBitsPaddr = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.dcache.missQueue.io_req_bits_addr")

  val mainPipeRefillReqValid = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.dcache.mainPipe.io_refill_req_valid")
  val mainPipeRefillReqReady = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.dcache.mainPipe.io_refill_req_ready")
  val mainPipeRefillReqBitsPaddr = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.dcache.mainPipe.io_refill_req_bits_addr")

  val mainPipeS2Valid = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.dcache.mainPipe.s2_valid")
  val mainPipeS2Ready = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.dcache.mainPipe.s2_ready")
  val mainPipeS2ReqVaddr = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.dcache.mainPipe.s2_req_vaddr")
  val mainPipeS2NeedEviction = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.memBlock.inner_dcache.dcache.mainPipe.s2_need_eviction")

  def loadValid(cycle: Int, slot: Int): Boolean = {
    loadReqValid(slot).get_at_time(cycle).asBoolean
  }

  def loadHasValid(cycle: Int): Boolean = {
    loadReqValid.map(_.get_at_time(cycle).asBoolean).reduce(_ || _)
  }

  def loadReady(cycle: Int, slot: Int): Boolean = {
    loadReqReady(slot).get_at_time(cycle).asBoolean
  }

  def loadFire(cycle: Int, slot: Int): Boolean = {
    loadReqValid(slot).get_at_time(cycle).asBoolean && loadReqReady(slot).get_at_time(cycle).asBoolean
  }

  def loadSlotMatchVaddr(cycle: Int, slot: Int, vaddr: BigInt): Boolean = {
    loadFire(cycle, slot) && (loadReqBitsVaddr(slot).get_at_time(cycle).toBigInt() >> 6 == (vaddr >> 6)) // compare only the cache line part
  }

  def loadMatchVaddr(cycle: Int, vaddr: BigInt): Int = {
    if (loadSlotMatchVaddr(cycle, 0, vaddr)) {
      0
    } else if (loadSlotMatchVaddr(cycle, 1, vaddr)) {
      1
    } else if (loadSlotMatchVaddr(cycle, 2, vaddr)) {
      2
    } else {
      -1 // No match found
    }
  }

  def missReqValid(cycle: Int): Boolean = {
    mainPipeMissReqValid.get_at_time(cycle).asBoolean
  }

  def missReqReady(cycle: Int): Boolean = {
    mainPipeMissReqReady.get_at_time(cycle).asBoolean
  }

  def missReqFire(cycle: Int): Boolean = {
    mainPipeMissReqValid.get_at_time(cycle).asBoolean && mainPipeMissReqReady.get_at_time(cycle).asBoolean
  }

  def missReqMatchVaddr(cycle: Int, vaddr: BigInt): Boolean = {
    (mainPipeMissReqBitsVaddr.get_at_time(cycle).toBigInt() >> 6) == (vaddr >> 6)
  }

  def missReqPaddr(cycle: Int): BigInt = {
    // here we assume that the miss request is valid
    mainPipeMissReqBitsPaddr.get_at_time(cycle).toBigInt()
  }

  def refillReqValid(cycle: Int): Boolean = {
    mainPipeRefillReqValid.get_at_time(cycle).asBoolean
  }

  def refillReqReady(cycle: Int): Boolean = {
    mainPipeRefillReqReady.get_at_time(cycle).asBoolean
  }

  def refillReqFire(cycle: Int): Boolean = {
    refillReqValid(cycle) && refillReqReady(cycle)
  }

  def refillReqPaddr(cycle: Int): BigInt = {
    // here we assume that the refill request is valid
    mainPipeRefillReqBitsPaddr.get_at_time(cycle).toBigInt()
  }

  def refillReqMatchPaddr(cycle: Int, paddr: BigInt): Boolean = {
    (refillReqPaddr(cycle) >> 6) == (paddr >> 6) // compare only the cache line part
  }

  def mainPipeS2Valid(cycle: Int): Boolean = {
    mainPipeS2Valid.get_at_time(cycle).asBoolean
  }

  def mainPipeS2Ready(cycle: Int): Boolean = {
    mainPipeS2Ready.get_at_time(cycle).asBoolean // assuming S2 is ready when refill request is ready
  }

  def mainPipeS2Fire(cycle: Int): Boolean = {
    mainPipeS2Valid(cycle) && mainPipeS2Ready(cycle)
  }

  def reqVaddr(cycle: Int): BigInt = {
    mainPipeS2ReqVaddr.get_at_time(cycle).toBigInt()
  }

  def reqNeedsEviction(cycle: Int): Boolean = {
    mainPipeS2NeedEviction.get_at_time(cycle).asBoolean
  }

  def reqMatchVaddr(cycle: Int, vaddr: BigInt): Boolean = {
   mainPipeS2Fire(cycle) && reqVaddr(cycle) == vaddr
  }
  
  def reqValid(cycle: Int): Boolean = {
    mainPipeS2Fire(cycle)
  }
}
