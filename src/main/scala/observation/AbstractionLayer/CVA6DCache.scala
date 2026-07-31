package HT.observation.AbstractionLayer

import HT.observation.{SignalReader, ParsedData}
import observation.AbstractionLayer.ObservationAbstractionBase

val CVA6LoadPort = 3
val CVA6MissUnitSize = 4

val CVA6DCacheSignalSet: Set[String] = (
  // Load/Store Ports Signals
  Seq((0 until CVA6LoadPort).map { i =>
    s"TOP.ariane_testharness.i_ariane.i_cva6.dcache_req_ports_ex_cache[$i].data_req"
  } ++
    (0 until CVA6LoadPort).map { i =>
      s"TOP.ariane_testharness.i_ariane.i_cva6.dcache_req_ports_cache_ex[$i].data_gnt"
    } ++
    (0 until CVA6LoadPort).map { i =>
      s"TOP.ariane_testharness.i_ariane.i_cva6.dcache_req_ports_ex_cache[$i].address_tag"
    } ++
    (0 until CVA6LoadPort).map { i =>
      s"TOP.ariane_testharness.i_ariane.i_cva6.dcache_req_ports_ex_cache[$i].address_index"
    } ++
    (0 until CVA6LoadPort).map { i =>
      s"TOP.ariane_testharness.i_ariane.i_cva6.dcache_req_ports_ex_cache[$i].data_we"
    }).flatten ++
    // Miss Unit Signals
    Seq(
      "TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_wt_dcache.i_wt_dcache_missunit.miss_req_i"
    ) ++
    (0 until CVA6MissUnitSize).map { i =>
      s"TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_wt_dcache.i_wt_dcache_missunit.miss_paddr_i[$i]"
    } ++
    // NEW: Miss ID Signals
    (0 until CVA6MissUnitSize).map { i =>
      s"TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_wt_dcache.i_wt_dcache_missunit.miss_id_i[$i]"
    } ++
    // Refill Signals
    Seq(
      "TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_wt_dcache.i_wt_dcache_missunit.mem_rtrn_vld_i",
      "TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_wt_dcache.i_wt_dcache_missunit.mem_rtrn_i.tid"
    )
  ).toSet

class CVA6DCache(wp: ParsedData) extends ObservationAbstractionBase {

  // --- Load Port Readers ---
  val loadReqValid = (0 until CVA6LoadPort).map { i =>
    SignalReader(wp, s"TOP.ariane_testharness.i_ariane.i_cva6.dcache_req_ports_ex_cache[$i].data_req")
  }
  val loadReqGnt = (0 until CVA6LoadPort).map { i =>
    SignalReader(wp, s"TOP.ariane_testharness.i_ariane.i_cva6.dcache_req_ports_cache_ex[$i].data_gnt")
  }
  val loadReqTag = (0 until CVA6LoadPort).map { i =>
    SignalReader(wp, s"TOP.ariane_testharness.i_ariane.i_cva6.dcache_req_ports_ex_cache[$i].address_tag")
  }
  val loadReqIndex = (0 until CVA6LoadPort).map { i =>
    SignalReader(wp, s"TOP.ariane_testharness.i_ariane.i_cva6.dcache_req_ports_ex_cache[$i].address_index")
  }

  // --- Miss Unit Readers ---
  val missReqValidMask = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_wt_dcache.i_wt_dcache_missunit.miss_req_i")

  val missReqPaddr = (0 until CVA6MissUnitSize).map { i =>
    SignalReader(wp, s"TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_wt_dcache.i_wt_dcache_missunit.miss_paddr_i[$i]")
  }

  // NEW: Miss ID Reader
  val missReqId = (0 until CVA6MissUnitSize).map { i =>
    SignalReader(wp, s"TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_wt_dcache.i_wt_dcache_missunit.miss_id_i[$i]")
  }

  // --- Refill Readers ---
  val refillValidReader = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_wt_dcache.i_wt_dcache_missunit.mem_rtrn_vld_i")
  val refillTid = SignalReader(wp, "TOP.ariane_testharness.i_ariane.i_cva6.gen_cache_wt.i_cache_subsystem.i_wt_dcache.i_wt_dcache_missunit.mem_rtrn_i.tid")


  // --- Helper Logic ---

  def loadValid(cycle: Int, slot: Int): Boolean = {
    loadReqValid(slot).get_at_time(cycle).asBoolean
  }

  def loadReady(cycle: Int, slot: Int): Boolean = {
    loadReqGnt(slot).get_at_time(cycle).asBoolean
  }

  def loadFire(cycle: Int, slot: Int): Boolean = {
    loadValid(cycle, slot) && loadReady(cycle, slot)
  }

  def loadSlotMatchVaddr(cycle: Int, slot: Int, vaddr: BigInt): Boolean = {
    if (!loadFire(cycle, slot)) return false
    val tag = loadReqTag(slot).get_at_time(cycle).toBigInt()
    val index = loadReqIndex(slot).get_at_time(cycle).toBigInt()
    val reconstructedAddr = (tag << 12) | index
    // println(s"vaddr ${reconstructedAddr.toString(16)} cmp $vaddr Cycle ${cycle} slot ${slot}")
    reconstructedAddr == vaddr
  }

  def loadMatchVaddr(cycle: Int, vaddr: BigInt): Int = {
    if (loadSlotMatchVaddr(cycle, 0, vaddr)) 0
    else if (loadSlotMatchVaddr(cycle, 1, vaddr)) 1
    else if (loadSlotMatchVaddr(cycle, 2, vaddr)) 2
    else -1
  }

  // --- Miss Unit Logic ---

  def missReqValid(cycle: Int, slot: Int): Boolean = {
    val mask = missReqValidMask.get_at_time(cycle).toBigInt()
    ((mask >> slot) & 1) == 1
  }

  def missReqPaddr(cycle: Int, slot: Int): BigInt = {
    missReqPaddr(slot).get_at_time(cycle).toBigInt()
  }

  // NEW: Helper to get the ID of a miss request at a specific slot
  def missReqId(cycle: Int, slot: Int): BigInt = {
    missReqId(slot).get_at_time(cycle).toBigInt()
  }

  def missReqMatchPaddr(cycle: Int, paddr: BigInt): Int = {
    val matches = (0 until CVA6MissUnitSize).filter { i =>
      missReqValid(cycle, i) && (missReqPaddr(cycle, i) >> 6) == (paddr >> 6)
    }
    matches.headOption.getOrElse(-1)
  }

  // NEW: Helper to find which miss slot (if any) matches a specific transaction ID
  def missReqMatchId(cycle: Int, id: BigInt): Int = {
    val matches = (0 until CVA6MissUnitSize).filter { i =>
      missReqValid(cycle, i) && missReqId(cycle, i) == id
    }
    matches.headOption.getOrElse(-1)
  }

  // --- Refill Logic ---

  def refillReqValid(cycle: Int): Boolean = {
    refillValidReader.get_at_time(cycle).asBoolean
  }

  def refillReqTid(cycle: Int): BigInt = {
    refillTid.get_at_time(cycle).toBigInt()
  }

  // NEW: Helper to check if a Refill TID matches a specific ID
  def refillMatchTid(cycle: Int, id: BigInt): Boolean = {
    refillReqValid(cycle) && refillReqTid(cycle) == id
  }
}