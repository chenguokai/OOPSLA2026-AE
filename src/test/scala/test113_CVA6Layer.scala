import HT.ASTNodes.*
import HT.ASTUtils.printAST
import HT.CachelineUtils.EvictSetFromL1DCacheline
import HT.CodeGen.*
import HT.Executors.pyStr2Result
import HT.Permissions.Permission.VictimPublic
import HT.StdLib.*
import HT.Types.*
import HT.observation.*
import HT.observation.AbstractionLayer.*
import HT.simrunners.XSRun
import HT.*
import HT.ASTNodes.given_Conversion_ValueNode_ArithNode
import HT.ASTNodes.given_Conversion_Int_ValueNode
import HT.ASTNodes.given_Conversion_Long_ValueNode
import HT.ASTNodes.given_Conversion_Int_ArithNode
import HT.given_Conversion_Long_PlacementOperator

def test113body = {
  val vcd_path = "/mnt/ssd4t/home/xim-intel14/cva6/cva6/verif/sim/out_2026-02-18/veri-testharness_sim/rv64ui-v-add.cv64a6_imafdc_sv39.vcd"
  val parser = VCDParser(vcd_path.toString, CVA6BPUSignalSet ++ CVA6CommitSignalSet ++ CVA6DCacheSignalSet ++ CVA6ICacheSignalSet ++ CVA6BufferSignalSet ++ CVA6RedirectSignalSet)

  val bpuObj = new CVA6BPU(parser)
  val commitObj = new CVA6Commit(parser)
  val dcacheObj = new CVA6DCache(parser)
  val icacheObj = new CVA6ICache(parser)
  val bufferObj = new CVA6IBuffer(parser)
  val redirectObj = new CVA6Redirect(parser)



  val end = () => {
    true
  }
  /*
  val bpuUpdateValid = () => {
    bpuObj.updateValid(GlobalCycle)
  }

  Range(bpuUpdateValid, end, "BPU update") {
    entry{
      EventLog(s"PC ${bpuObj.updatePc(GlobalCycle).toString(16)}, target ${if (bpuObj.updateTargets(GlobalCycle).head.isDefined) bpuObj.updateTargets(GlobalCycle).head.get.toString(16) else "None"}, taken ${bpuObj.updateTaken(GlobalCycle)}")
    }
  }
  */
  /*
  val bpuPredictValid = () => {
    bpuObj.lastOutValid((GlobalCycle))
  }

  Range(bpuPredictValid, end, "BPU predict") {
    entry{
      EventLog(s"PC ${bpuObj.lastOutPc(GlobalCycle).toString(16)} Taken ${bpuObj.lastOutTaken(GlobalCycle)}, Target ${bpuObj.lastOutTarget(GlobalCycle).toString(16)}")
    }
  }
  */
  /*
  val commitInstValid = () => {
    commitObj.commitValid(GlobalCycle, 0)
  }

  Range(commitInstValid, end, "Inst commit") {
    entry{
      EventLog(s"PC ${commitObj.getPC(GlobalCycle, 0).toString(16)} isCompressed ${commitObj.isCompressed(GlobalCycle, 0)}")
    }
  }
   */

  /*
  val loadMatchAddr = () => {
    dcacheObj.loadMatchVaddr(GlobalCycle, 0x80009018L) != -1
  }

  Range(loadMatchAddr, end, "Load MatchAddr") {
    entry{
      EventLog("req at cycle " + GlobalCycle)
    }
  }
   */
  /*
  val loadMissValid = () => {
    dcacheObj.missReqValid(GlobalCycle, 0)
  }

  Range(loadMissValid, end, "load miss valid") {
    entry {
      EventLog(s"cycle $GlobalCycle paddr ${dcacheObj.missReqPaddr(GlobalCycle, 0).toString(16)}, id ${dcacheObj.missReqId(GlobalCycle, 0)}")
    }
  }
   */
  /*
  val loadRefillValid = () => {
    dcacheObj.refillReqValid(GlobalCycle)
  }

  Range(loadRefillValid, end, "Load refill come") {
    entry{
      EventLog(s"cycle $GlobalCycle tid ${dcacheObj.refillReqTid(GlobalCycle)}")
    }
  }
   */
  /*
  val icacheAcquireValid = () => {
    icacheObj.memAcquireValid(GlobalCycle)
  }

  Range(icacheAcquireValid, end, "Icache acquire") {
    entry {
      EventLog(s"Paddr ${icacheObj.memAcquirePAddress(GlobalCycle).toString(16)} source ${icacheObj.memAcquireSource(GlobalCycle)}")
    }
  }
   */
  /*
  val icacheGrantValid = () => {
    icacheObj.memGrantValid(GlobalCycle)
  }

  Range(icacheGrantValid, end, "icache grant") {
    entry{
      EventLog(s"grant source ${icacheObj.memGrantSource(GlobalCycle)}")
    }
  }*/
  /*
  val fetchValid = () => {
    icacheObj.TLBReqValid(GlobalCycle)
  }

  Range(fetchValid, end, "fetch request") {
    entry {
      EventLog(s"Vaddr ${icacheObj.TLBFireVaddr(GlobalCycle).toString(16)} Paddr ${icacheObj.TLBFirePaddr(GlobalCycle).toString(16)}, exception ${icacheObj.TLBRespFault(GlobalCycle)}")
    }
  }
   */

  /*
  val ibufferSlot0Valid = () => {
    bufferObj.isSlotValid(GlobalCycle, 0)
  }

  Range(ibufferSlot0Valid, end, "Ibuffer valid", maxCycle = 10000) {
    entry{
      EventLog(s"PC ${bufferObj.getPC(GlobalCycle, 0).toString(16)}, isControlFlow ${bufferObj.isControlFlow(GlobalCycle, 0)}")
    }
  }
   */
  val redirectValid = () => {
    redirectObj.hasRedirect(GlobalCycle)
  }

  Range(redirectValid, end, "Redirect", maxCycle = 10000) {
    entry{
      EventLog(s"PC ${redirectObj.redirectPC(GlobalCycle).toString(16)}, isTaken ${redirectObj.isTaken(GlobalCycle)}, redirectTarget ${redirectObj.redirectTargetAddress(GlobalCycle).toString(16)}")
    }
  }
}

@main def TestCVA6Layer_CVA6 = {
  test113body
}