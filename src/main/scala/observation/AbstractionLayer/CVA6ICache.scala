package HT.observation.AbstractionLayer

import HT.observation.{SignalReader, ParsedData}
import observation.AbstractionLayer.ObservationAbstractionBase

val CVA6ICacheSignalSet: Set[String] = Set(
  // ICache TLB req and resp
  "TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_cva6_icache.areq_o.fetch_req",
  "TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_cva6_icache.areq_o.fetch_vaddr",
  "TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_cva6_icache.areq_i.fetch_valid",
  "TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_cva6_icache.areq_i.fetch_paddr",
  "TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_cva6_icache.areq_i.fetch_exception.cause",

  // ICache req (Memory Acquire)
  "TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_cva6_icache.mem_data_req_o",
  "TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_cva6_icache.mem_data_o.paddr",
  "TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_cva6_icache.mem_data_o.tid",

  // ICache resp (Memory Grant/Return)
  "TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_cva6_icache.mem_rtrn_vld_i",
  "TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_cva6_icache.mem_rtrn_i.tid",
  "TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_cva6_icache.mem_rtrn_i.data"
)

class CVA6ICache(wp: ParsedData) extends ObservationAbstractionBase {

  // --- Memory Interface (Acquire) ---
  val memDataReqValid = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_cva6_icache.mem_data_req_o")
  val memDataPAddr    = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_cva6_icache.mem_data_o.paddr")
  val memDataTid      = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_cva6_icache.mem_data_o.tid")

  // --- Memory Interface (Grant/Return) ---
  val memRtrnValid    = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_cva6_icache.mem_rtrn_vld_i")
  val memRtrnTid      = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_cva6_icache.mem_rtrn_i.tid")
  // val memRtrnData  = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_cva6_icache.mem_rtrn_i.data") // Available if data inspection is needed

  // --- TLB / Fetch Interface ---
  val fetchReq        = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_cva6_icache.areq_o.fetch_req")
  val fetchVAddr      = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_cva6_icache.areq_o.fetch_vaddr")
  val fetchValid      = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_cva6_icache.areq_i.fetch_valid")
  val fetchPAddr      = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_cva6_icache.areq_i.fetch_paddr")
  val fetchException  = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_cva6_icache.areq_i.fetch_exception.cause")


  // --- Memory Abstraction Methods ---

  def memAcquireValid(cycle: Int): Boolean = {
    memDataReqValid.get_at_time(cycle).asBoolean
  }

  // Note: CVA6 signals provided do not include a 'ready' signal for the request.
  // We assume the request is valid if the valid bit is set.
  def memAcquireFire(cycle: Int): Boolean = {
    memAcquireValid(cycle)
  }

  def memAcquirePAddress(cycle: Int): BigInt = {
    memDataPAddr.get_at_time(cycle).toBigInt()
  }

  def memAcquireSource(cycle: Int): BigInt = {
    memDataTid.get_at_time(cycle).toBigInt()
  }

  def memAcquireFireWithPAddress(cycle: Int, paddr: BigInt): Boolean = {
    memAcquireFire(cycle) && memAcquirePAddress(cycle) == paddr
  }

  def memGrantValid(cycle: Int): Boolean = {
    memRtrnValid.get_at_time(cycle).asBoolean
  }

  def memGrantSource(cycle: Int): BigInt = {
    memRtrnTid.get_at_time(cycle).toBigInt()
  }


  // --- TLB Abstraction Methods ---

  // Mapping 'areq' interface to TLB abstractions

  def TLBReqValid(cycle: Int): Boolean = {
    fetchReq.get_at_time(cycle).asBoolean
  }

  def TLBFire(cycle: Int): Boolean = {
    TLBReqValid(cycle)
  }

  def TLBFireVaddr(cycle: Int): BigInt = {
    fetchVAddr.get_at_time(cycle).toBigInt()
  }

  def TLBFirePC(cycle: Int, pc: BigInt): Boolean = {
    TLBFire(cycle) && fetchVAddr.get_at_time(cycle - 1).toBigInt() == pc
  }

  def TLBRespFault(cycle: Int): Boolean = {
    // If fetch_valid is High but we have an exception cause, or if logic dictates checking exception separately.
    // Based on signals, we check if there is a non-zero exception cause when valid.
    val cause = fetchException.get_at_time(cycle).toBigInt()
    cause != 0
  }

  def TLBFirePaddr(cycle: Int): BigInt = {
    fetchPAddr.get_at_time(cycle).toBigInt()
  }
}