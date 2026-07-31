package HT.observation.AbstractionLayer

import HT.observation.{SignalReader, ParsedData}
import observation.AbstractionLayer.ObservationAbstractionBase

val XSFTQSignalSet = Set(
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_ftq.io_toIfu_redirect_valid",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_ftq.io_toIfu_redirect_bits_ftqIdx_flag",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_ftq.io_toIfu_redirect_bits_ftqIdx_value",
  "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_ftq.io_toIfu_redirect_bits_ftqOffset"
)

class XSFTQ(wp: ParsedData) extends ObservationAbstractionBase {
  val redirectValid = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_ftq.io_toIfu_redirect_valid")
  val redirectFTQPtrFlag = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_ftq.io_toIfu_redirect_bits_ftqIdx_flag")
  val redirectFTQPtrValue = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_ftq.io_toIfu_redirect_bits_ftqIdx_value")
  val redirectFTQOffset = SignalReader(wp, "TOP.SimTop.l_soc.core_with_l2.core.frontend.inner_ftq.io_toIfu_redirect_bits_ftqOffset")

  def hasRedirect(cycle: Int): Boolean = {
    redirectValid.get_at_time(cycle).asBoolean
  }

  def redirectFTQPtr(cycle: Int): (BigInt, BigInt) = {
    (redirectFTQPtrFlag.get_at_time(cycle).toBigInt(), redirectFTQPtrValue.get_at_time(cycle).toBigInt())
  }

  def redirectFTQOffset(cycle: Int): BigInt = {
    redirectFTQOffset.get_at_time(cycle).toBigInt()
  }

  def redirectIsOlderThan(cycle: Int, flag: BigInt, value: BigInt, offset: BigInt): Boolean = {
    val (redirectFlag, redirectValue) = redirectFTQPtr(cycle)
    val redirectOffsetValue = redirectFTQOffset(cycle)
    // circular queue ptr comparison
    if (redirectFlag == flag && redirectValue == value) {
      redirectOffsetValue < offset
    } else {
      (redirectFlag != flag && redirectValue > value) || (redirectFlag == flag && redirectValue < value)
    }
  }

  def redirectIsYoungerThan(cycle: Int, flag: BigInt, value: BigInt, offset: BigInt): Boolean = {
    val (redirectFlag, redirectValue) = redirectFTQPtr(cycle)
    val redirectOffsetValue = redirectFTQOffset(cycle)
    // circular queue ptr comparison
    if (redirectFlag == flag && redirectValue == value) {
      redirectOffsetValue > offset
    } else {
      (redirectFlag != flag && redirectValue < value) || (redirectFlag == flag && redirectValue > value)
    }
  }

  def isOlderThan(AFlag: BigInt, AValue: BigInt, AOffset: BigInt, BFlag: BigInt, BValue: BigInt, BOffset: BigInt): Boolean = {
    // A is older than B
    if (AFlag == BFlag && AValue == BValue) {
      AOffset < BOffset
    } else {
      (AFlag != BFlag && AValue > BValue) || (AFlag == BFlag && AValue < BValue)
    }
  }
  
  def isYoungerThan(AFlag: BigInt, AValue: BigInt, AOffset: BigInt, BFlag: BigInt, BValue: BigInt, BOffset: BigInt): Boolean = {
    // A is younger than B
    if (AFlag == BFlag && AValue == BValue) {
      AOffset > BOffset
    } else {
      (AFlag != BFlag && AValue < BValue) || (AFlag == BFlag && AValue > BValue)
    }
  }

}
