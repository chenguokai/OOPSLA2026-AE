package observation.AbstractionLayer

import HT.observation.{SignalReader, ParsedData}

class XSIFU(wp: ParsedData) extends ObservationAbstractionBase {
  val reqValid = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.frontend.inner_ifu.io_ftqInter_fromFtq_req_valid")
  val reqReady = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.frontend.inner_ifu.io_ftqInter_fromFtq_req_ready")
  val reqStartAddr = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.frontend.inner_ifu.io_ftqInter_fromFtq_req_bits_startAddr")
  val tlbReqValid = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.frontend.inner_ifu.io_iTLBInter_req_valid")
  val tlbReqReady = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.frontend.inner_ifu.io_iTLBInter_req_ready")
  val tlbRespValid = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.frontend.inner_ifu.io_iTLBInter_resp_valid")
  val tlbRespReady = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.frontend.inner_ifu.io_iTLBInter_resp_ready")
  val tlbRespBitsPaddr = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.frontend.inner_ifu.io_iTLBInter_resp_bits_paddr_0")

}
