package HT.observation
import scala.io.Source
import HT.observation.SignalVector
import scala.collection.immutable.SortedMap

class EventReader(wp:ParsedGem5Data, signalName: String) {
    def get_at_time(time: Long): Long = {
        val signal = wp.getSignal(signalName)
        if (signal.isEmpty || signal.get(time).isEmpty) {
            throw new NoSuchElementException(s"Signal $signalName not found.")
        } else if (signal.get(time).size > 1) {
            signal.get(time).head
            // throw new IllegalStateException(s"Signal $signalName has multiple events at time $time.")
        } else {
            signal.get(time).head
        } 
    }
}

class EventListReader(wp: ParsedGem5Data, signalName: String) {
    def get_at_time(time: Long): Set[Long] = {
        val signal = wp.getSignal(signalName)
        if (signal.isEmpty || signal.get(time).isEmpty) {
            throw new NoSuchElementException(s"Signal $signalName not found.")
        } else {
            signal.get(time)
        }
    }
}

class EventPresentReader(wp: ParsedGem5Data, signalName: String) {
    def get_at_time(time: Long): Boolean = {
        val signal = wp.getSignal(signalName)
        signal.exists(_.contains(time))
    }
}

class SignalReader(sp: ParsedData, signalName: String) extends WaveformConsumer {
    assert(sp.getSignals.keySet.contains(signalName), s"Signal $signalName not found in the waveform parser.")
    
    def get_at_time(time: Long): SignalVector = {
        val signal = sp.getSignals.get(signalName)
        if (signal.isEmpty) {
            throw new NoSuchElementException(s"Signal $signalName not found.")
        } else {
            val sm = signal.get
            sm.maxBefore(time).getOrElse(
                (time, SignalVector("x", sm.head._2.size))
            )._2
        } 
    }

    def getUsedSignals: Set[String] = {
        Set(signalName)
    }
}


