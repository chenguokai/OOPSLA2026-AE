package HT.observation

import HT.observation.{SignalVector, ParsedData}

import scala.collection.BufferedIterator
import scala.collection.immutable.SortedMap
import scala.collection.mutable.{Map as MutableMap, SortedMap as MutableSortedMap}
import scala.io.Source

class VCDParserLimitedNewline(filename: String, sgns: Set[String]) extends ParsedData {

  val top_level_handlers: Map[String, Iterator[String] => Unit] = Map(
    "$timescale" -> handleTimescale,
    "$scope"     -> handleScope,
    "$comment" -> exhaustIterator,
    "$upscope"   -> handleUpScope,
    "$var"       -> handleVar,
    "$date" -> exhaustIterator,
    "$enddefinitions" -> exhaustIterator,
    "$verilog" -> exhaustIterator,
    )
    // Add more handlers as needed

  var current_scopes : List[String] = List.empty
  var current_time: Long = 0;

  import java.io.*

  def parse(filename: String, handlers: Map[String, Iterator[String] => Unit]): Map[String, SortedMap[Long, SignalVector]] = {

    val file = new java.io.File(filename)

    val tokens = Source.fromFile(file).getLines().buffered
    var inPreamble = true
    while (tokens.hasNext && inPreamble) {
      val token = tokens.head
      if (handlers.contains(token)) {
        val keyword = tokens.next()
        handlers(keyword)(new Iterator[String] {
          def hasNext: Boolean = tokens.hasNext && !ended
          def next(): String = {
            val t = tokens.next()
            if (t == "$end") ended = true
            t
          }
          private var ended = false
        })
        if keyword == "$enddefinitions" then {
          //info("End of definitions")
          inPreamble = false
        }
      } else {
        tokens.next() // skip non-keyword tokens
      }
    }

    val mut_signals: MutableMap[String, MutableSortedMap[Long, SignalVector]] = MutableMap.empty
    while (tokens.hasNext) {
      val token = tokens.next()
       if token.startsWith("#") then {
        current_time = token.substring(1).toInt
      } else if (token.startsWith("b") || token.startsWith("B")) {
        val actual_num = token.substring(1)
        val identifier = tokens.next()
        if wires.contains(identifier) then {
          val (signalName, signalSize) = wires(identifier)
          val signalVector = SignalVector(actual_num, signalSize)
          if !mut_signals.contains(signalName) then {
            mut_signals(signalName) = MutableSortedMap.empty
          }
          mut_signals(signalName)(current_time) = signalVector
        } else {
          //warning(s"Unknown identifier: $identifier")
        }
      } else if (token.startsWith("0") || token.startsWith("1") || token.startsWith("x") || token.startsWith("z") ||
              token.startsWith("X") || token.startsWith("Z")) {
        val value = token.charAt(0)
        val identifier = token.substring(1)
        if wires.contains(identifier) then {
          val (signalName, signalSize) = wires(identifier)
          val signalVector = SignalVector(value.toString(), signalSize)
          if !mut_signals.contains(signalName) then {
            mut_signals(signalName) = MutableSortedMap.empty
          }
          mut_signals(signalName)(current_time) = signalVector
        } else {
          //warning(s"Unknown identifier: $identifier")
        }
      }
    }

    mut_signals.map{  case (k, v) => k -> SortedMap.from(v)}.toMap

  }

  def handleTimescale(tokens: Iterator[String]): Unit = {
    //info(s"Timescale: ${tokens.mkString(" ")}")
  }

  def handleScope(tokens: Iterator[String]): Unit = {
    val scopeDetails = tokens.toList
    if scopeDetails.length == 3 then {
      val scopeType = scopeDetails.head
      val scopeName = scopeDetails(1)
      if scopeType != "module" then {
        //warning(s"Unknown scope type: $scopeType")
      } 
      current_scopes = scopeName :: current_scopes 
      //debug(s"Scope: $scopeType $scopeName")
    } else {
      //warning(s"Invalid scope definition: ${scopeDetails.mkString(" ")}")
    }
  }

  def handleUpScope(tokens: Iterator[String]): Unit = {
    if current_scopes.nonEmpty then {
      current_scopes = current_scopes.tail
    } else {
      //warning("No scopes to pop")
    }
  }

  def handleVar(tokens: Iterator[String]): Unit = {
    val varDetails = tokens.toList
    if varDetails.length == 5 || varDetails.length == 6 then {
      val varType = varDetails.head
      val varSize = varDetails(1).toInt
      val varId = varDetails(2)
      val varName = varDetails(3)


      val scope_name = current_scopes.reverse.mkString(".")
      val fullVarName = s"$scope_name.$varName"
      if sgns.contains(fullVarName) then {
        wires(varId) = (fullVarName, varSize)

        if varType != "wire" && varType != "reg" then {
          //warning(s"Unknown variable type: $varType")
        }
      }
    } else {
      //error(s"Invalid variable definition: ${varDetails.mkString(" ")}, SKIPPING")
    }
  }

  def exhaustIterator(tokens: Iterator[String]): Unit = {
    while (tokens.hasNext) {
      tokens.next()
    }
  }


  val wires: MutableMap[String, (String, Int)] = MutableMap.empty

  val signals = parse(
    filename,
    top_level_handlers
  )

  def getSignals: Map[String, SortedMap[Long, SignalVector]] = {
    signals
  }

  def getSignal(signalName: String): Option[SortedMap[Long, SignalVector]] = {
    signals.get(signalName).map(SortedMap.from)
  }

  def getSignalNames: Set[String] = {
    signals.keySet.toSet
  }

  def getMaxTime: Long = {
    current_time
  }
}