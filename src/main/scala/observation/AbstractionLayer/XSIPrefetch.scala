package HT.observation.AbstractionLayer

import HT.observation.{SignalReader, ParsedData}
import observation.AbstractionLayer.ObservationAbstractionBase

val XSIPrefetchSignalSet: Set[String] = (
  Seq("TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.s2_valid",
    "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.s2_ready",
    "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.s2_req_vaddr_0",
    "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.s2_req_vaddr_1",
    "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.s2_doubleline",
    "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.s2_miss_0",
    "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.s2_miss_1"
  )
  ).toSet

class XSIPrefetch(wp: ParsedData) extends ObservationAbstractionBase {
  val memReplaceValid = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.s2_valid")
  val memReplaceReady = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.s2_ready")
  val memReplaceVAddress0 = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.s2_req_vaddr_0")
  val memReplaceVAddress1 = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.s2_req_vaddr_1")
  val memReqDoubleline = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.s2_doubleline")
  val memReplaceMiss0 = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.s2_miss_0")
  val memReplaceMiss1 = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.s2_miss_1")


  def prefetchReqValid(cycle: Int): Boolean = {
    memReplaceValid.get_at_time(cycle).asBoolean
  }
  def prefetchReqReady(cycle: Int): Boolean = {
    memReplaceReady.get_at_time(cycle).asBoolean
  }

  def prefetchReqFire(cycle: Int): Boolean = {
    prefetchReqValid(cycle) && prefetchReqReady(cycle)
  }

  def prefetchMiss0(cycle: Int): Boolean = {
    memReplaceMiss0.get_at_time(cycle).asBoolean
  }

  def prefetchMiss1(cycle: Int): Boolean = {
    memReplaceMiss1.get_at_time(cycle).asBoolean
  }

  def prefetchReqVAddr(cycle: Int): BigInt = {
    val vaddr0 = memReplaceVAddress0.get_at_time(cycle).toBigInt()
    vaddr0
  }

  def prefetchDoubleline(cycle: Int): Boolean = {
    memReqDoubleline.get_at_time(cycle).asBoolean
  }

}
