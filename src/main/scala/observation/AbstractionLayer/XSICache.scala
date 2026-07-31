package HT.observation.AbstractionLayer

import HT.observation.{SignalReader, ParsedData}
import observation.AbstractionLayer.ObservationAbstractionBase

val XSICacheITLBPort = 2

val XSICacheSignalSet: Set[String] = (
  Seq("TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.missUnit.io_mem_acquire_valid",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.missUnit.io_mem_acquire_ready",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.missUnit.io_mem_acquire_bits_address",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.missUnit.io_mem_acquire_bits_source",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.missUnit.io_mem_grant_valid",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.missUnit.io_mem_grant_bits_source",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_req_valid",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_req_ready",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_req_bits_startAddr",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_req_bits_nextlineStart")
  ++ Seq((0 until XSICacheITLBPort).map { i =>
    s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_itlb_${i}_req_valid"
  } ++
  (0 until XSICacheITLBPort).map { i =>
    s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_itlb_${i}_req_bits_vaddr"
  } ++
  (0 until XSICacheITLBPort).map { i =>
    s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_itlb_${i}_resp_bits_miss"
  } ++
  (0 until XSICacheITLBPort).map { i =>
    s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_itlb_${i}_resp_bits_paddr_0"
  } ++
  (0 until XSICacheITLBPort).map { i =>
    s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_itlb_${i}_resp_bits_excp_0_af_instr"
  } ++
  (0 until XSICacheITLBPort).map { i =>
    s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_itlb_${i}_resp_bits_excp_0_pf_instr"
  } ++
  (0 until XSICacheITLBPort).map { i =>
    s"TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_itlb_${i}_resp_bits_excp_0_gpf_instr"
  }).flatten
).toSet

class XSICache(wp: ParsedData) extends ObservationAbstractionBase {
  val memAcquireValid = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.missUnit.io_mem_acquire_valid")
  val memAcquireReady = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.missUnit.io_mem_acquire_ready")
  val memAcquirePAddress = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.missUnit.io_mem_acquire_bits_address")
  val memAcquireSource = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.missUnit.io_mem_acquire_bits_source")
  val memGrantValid = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.missUnit.io_mem_grant_valid")
  val memGrantSource = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.missUnit.io_mem_grant_bits_source")

  val prefetcherReqValid = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_req_valid")
  val prefetcherReqReady = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_req_ready")
  val prefetcherReqAddr = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_req_bits_startAddr")
  val prefetcherReqNextAddr = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_req_bits_nextlineStart")

  val tlb0ReqValid = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_itlb_0_req_valid")
  val tlb0ReqVaddr = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_itlb_0_req_bits_vaddr")
  val tlb0RespMiss = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_itlb_0_resp_bits_miss")
  val tlb0RespPaddr = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_itlb_0_resp_bits_paddr_0")
  val tlb0RespFaultA = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_itlb_0_resp_bits_excp_0_af_instr")
  val tlb0RespFaultP = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_itlb_0_resp_bits_excp_0_pf_instr")
  val tlb0RespFaultG = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_itlb_0_resp_bits_excp_0_gpf_instr")

  val tlb1ReqValid = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_itlb_1_req_valid")
  val tlb1ReqVaddr = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_itlb_1_req_bits_vaddr")
  val tlb1RespMiss = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_itlb_1_resp_bits_miss")
  val tlb1RespPaddr = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_itlb_1_resp_bits_paddr_0")
  val tlb1RespFaultA = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_itlb_1_resp_bits_excp_0_af_instr")
  val tlb1RespFaultP = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_itlb_1_resp_bits_excp_0_pf_instr")
  val tlb1RespFaultG = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_icache.prefetcher.io_itlb_1_resp_bits_excp_0_gpf_instr")


  def prefetchReqValid(cycle: Int): Boolean = {
    prefetcherReqValid.get_at_time(cycle).asBoolean
  }
  def prefetchReqReady(cycle: Int): Boolean = {
    prefetcherReqReady.get_at_time(cycle).asBoolean
  }

  def prefetchReqFire(cycle: Int): Boolean = {
    prefetchReqValid(cycle) && prefetchReqReady(cycle)
  }

  def prefetchReqRange(cycle: Int): (Long, Long) = {
    // mask cacheline offsets
    val startAddr = prefetcherReqAddr.get_at_time(cycle).toBigInt().toLong & 0xFFFFFFFFFFFFFFC0L
    val nextAddr = (prefetcherReqNextAddr.get_at_time(cycle).toBigInt().toLong & 0xFFFFFFFFFFFFFFC0L) + 64L // next line start is always +64 bytes from the start address
    (startAddr, nextAddr) // [) range
  }

  def prefetchWithinRange(cycle: Int, addr: Long): Boolean = {
    val (startAddr, nextAddr) = prefetchReqRange(cycle)
    addr >= startAddr && addr < nextAddr
  }

  def TLB0ReqValid(cycle: Int): Boolean = {
    tlb0ReqValid.get_at_time(cycle).asBoolean
  }
  def TLB1ReqValid(cycle: Int): Boolean = {
    tlb1ReqValid.get_at_time(cycle).asBoolean
  }

  def TLB0Fire(cycle: Int): Boolean = {
    // we do not care cases that previous cycle TLB0ReqValid but this cycle returned a miss
    TLB0ReqValid(cycle - 1) && !(tlb0RespMiss.get_at_time(cycle).asBoolean)
  }

  def TLB0FirePC(cycle: Int, pc: BigInt): Boolean = {
    TLB0Fire(cycle) && tlb0ReqVaddr.get_at_time(cycle - 1).toBigInt() == pc
  }

  def TLB1Fire(cycle: Int): Boolean = {
    TLB1ReqValid(cycle - 1) && !(tlb1RespMiss.get_at_time(cycle).asBoolean)
  }

  def TLB1FirePC(cycle: Int, pc: BigInt): Boolean = {
    TLB1Fire(cycle) && tlb1ReqVaddr.get_at_time(cycle - 1).toBigInt() == pc
  }

  def TLB0RespFault(cycle: Int): Boolean = {
    tlb0RespFaultA.get_at_time(cycle).asBoolean ||
    tlb0RespFaultP.get_at_time(cycle).asBoolean ||
    tlb0RespFaultG.get_at_time(cycle).asBoolean
  }

  def TLB1RespFault(cycle: Int): Boolean = {
    tlb1RespFaultA.get_at_time(cycle).asBoolean ||
    tlb1RespFaultP.get_at_time(cycle).asBoolean ||
    tlb1RespFaultG.get_at_time(cycle).asBoolean
  }

  def TLB0FirePaddr(cycle: Int): BigInt = {
    // here we assume that TLB0Fire and this are called in the same cycle
    tlb0RespPaddr.get_at_time(cycle).toBigInt()
  }

  def TLB1FirePaddr(cycle: Int): BigInt = {
    // here we assume that TLB1Fire and this are called in the same cycle
    tlb1RespPaddr.get_at_time(cycle).toBigInt()
  }

  def memAcquireValid(cycle: Int): Boolean = {
    memAcquireValid.get_at_time(cycle).asBoolean
  }
  def memAcquireReady(cycle: Int): Boolean = {
    memAcquireReady.get_at_time(cycle).asBoolean
  }
  def memAcquireFire(cycle: Int): Boolean = {
    memAcquireValid(cycle) && memAcquireReady(cycle)
  }
  def memAcquirePAddress(cycle: Int): BigInt = {
    val v = memAcquirePAddress.get_at_time(cycle).toBigInt()
    // println(s"memAcquirePAddress at cycle $cycle: $v")
    v
  }
  def memAcquireSource(cycle: Int): BigInt = {
    memAcquireSource.get_at_time(cycle).toBigInt()
  }
  def memAcquireFireWithPAddress(cycle: Int, paddr: BigInt): Boolean = {
    memAcquireFire(cycle) && memAcquirePAddress(cycle) == paddr
  }
  
  def memGrantValid(cycle: Int): Boolean = {
    memGrantValid.get_at_time(cycle).asBoolean
  }
  
  def memGrantSource(cycle: Int): BigInt = {
    memGrantSource.get_at_time(cycle).toBigInt()
  }

}
