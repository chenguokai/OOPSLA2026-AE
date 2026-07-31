package HT.observation

import scala.collection.immutable.SortedMap

trait ParsedData {
  def getSignals: Map[String, SortedMap[Long, SignalVector]] 
  def getSignal(signalName: String): Option[SortedMap[Long, SignalVector]]
  def getSignalNames: Set[String]
  def getMaxTime: Long
}

trait ParsedGem5Data {
  def getSignal(signalName: String): Option[SortedMap[Long, Set[Long]]]
  def getMaxTime: Long
}