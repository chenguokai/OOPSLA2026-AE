package HT.observation
trait WaveformConsumer {
  def getUsedSignals: Set[String]
}
