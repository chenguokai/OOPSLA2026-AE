package HT.observation.AbstractionLayer

import HT.observation.{EventListReader, EventPresentReader, EventReader, ParsedGem5Data}
import observation.AbstractionLayer.ObservationAbstractionBase

import scala.math.BigDecimal.long2bigDecimal

class Gem5O3(wp: ParsedGem5Data) extends ObservationAbstractionBase {
  // wp: event log path
  val commitValid = EventPresentReader(wp, "A")
  val commitPc = EventListReader(wp, "A")
  val raxReg = EventReader(wp, "B")
  val mispredictValid = EventPresentReader(wp, "C")
  val mispredictPc = EventReader(wp, "C")
  val icacheReqValid = EventPresentReader(wp, "I")
  val icacheReqAddr = EventReader(wp, "I")
  val fetchValid = EventPresentReader(wp, "IB")
  val fetchPc = EventListReader(wp, "IB")
  val predValid = EventPresentReader(wp, "BP")
  val predPc = EventReader(wp, "BP")
  val predTaken = EventReader(wp, "BPD")
  val predTarget = EventReader(wp, "BPT")
  val dcacheReqValid = EventPresentReader(wp, "D")
  val dcacheReqAddr = EventListReader(wp, "D")
  val dcacheReqHit = EventReader(wp, "DH")


  def hasMispred(cycle: Int): Boolean = {
    mispredictValid.get_at_time(cycle)
  }

  def MispredPC(cycle: Int): BigInt = {
    mispredictPc.get_at_time(cycle).toBigInt
  }

  def hasCommit(cycle: Int): Boolean = {
    commitValid.get_at_time(cycle)
  }

  def commitWithPC(cycle: Int, pc: Long): Boolean = {
    hasCommit(cycle) && commitPc.get_at_time(cycle).contains(pc)
  }

  def getRax(cycle: Int): BigInt = {
    raxReg.get_at_time(cycle).toBigInt
  }

  def hasICacheReq(cycle: Int): Boolean = {
    icacheReqValid.get_at_time(cycle)
  }

  def getICacheReqAddr(cycle: Int): BigInt = {
    icacheReqAddr.get_at_time(cycle).toBigInt
  }

  def hasFetch(cycle: Int): Boolean = {
    fetchValid.get_at_time(cycle)
  }

  def fetchPCMatchPC(cycle: Int, pc: Long): Boolean = {
    hasFetch(cycle) && fetchPc.get_at_time(cycle).contains(pc)
  }

  def getFetchPC(cycle: Int): Set[BigInt] = {
    if (hasFetch(cycle)) {
      fetchPc.get_at_time(cycle).map(_.toBigInt)
    } else {
      Set.empty
    }
  }

  def hasPred(cycle: Int): Boolean = {
    predValid.get_at_time(cycle)
  }

  def getPredPC(cycle: Int): BigInt = {
    predPc.get_at_time(cycle).toBigInt
  }

  def getPredTaken(cycle: Int): Boolean = {
    predTaken.get_at_time(cycle) == 0
  }

  def getPredTarget(cycle: Int): BigInt = {
    predTarget.get_at_time(cycle).toBigInt
  }

  def hasDCacheReq(cycle: Int): Boolean = {
    dcacheReqValid.get_at_time(cycle)
  }

  def getDCacheReqAddr(cycle: Int): Set[BigInt] = {
    if (hasDCacheReq(cycle)) {
      dcacheReqAddr.get_at_time(cycle).map(_.toBigInt)
    } else {
      Set.empty
    }
  }

  def getDCacheReqAddrMatch(cycle: Int, addr: Long): Boolean = {
    hasDCacheReq(cycle) && dcacheReqAddr.get_at_time(cycle).contains(addr)
  }

  def isDCacheHit(cycle: Int): Boolean = {
    dcacheReqHit.get_at_time(cycle) == 1
  }
}
