package HT.observation

import HT.observation.{SignalVector, ParsedData}
import sun.security.ssl.SSLLogger.warning

import scala.collection.immutable.SortedMap
import scala.collection.BufferedIterator
import scala.collection.mutable.{Map as MutableMap, SortedMap as MutableSortedMap, Set as MutableSet}

var current_parser_time: Long = 0;

class VCDParser(filename: String, sgns: Set[String]) extends ParsedData {

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

  import java.io._
  /*
  def whitespaceIterator(file: File): BufferedIterator[String] = new BufferedIterator[String] {
    private val reader = new BufferedReader(new FileReader(file))
    private val buf = new StringBuilder
    private var nextChar = -1
    private var nextToken: Option[String] = None

    private def readNextChar(): Int = {
      nextChar = reader.read()
      nextChar
    }

    private def fetchNextToken(): Unit = {
      buf.clear()
      while (nextChar != -1 && nextChar.toChar.isWhitespace)
        readNextChar()
      while (nextChar != -1 && !nextChar.toChar.isWhitespace) {
        buf.append(nextChar.toChar)
        readNextChar()
      }
      nextToken = if (buf.isEmpty) None else Some(buf.toString)
    }

    readNextChar()
    fetchNextToken()

    def hasNext: Boolean = nextToken.nonEmpty

    def head: String = {
      if (!hasNext) throw new NoSuchElementException
      nextToken.get
    }

    def next(): String = {
      val res = nextToken.getOrElse(throw new NoSuchElementException)
      fetchNextToken()
      res
    }

    def close(): Unit = reader.close()
  }

*/

  def whitespaceIterator(file: File): BufferedIterator[String] = new BufferedIterator[String] {
    private val reader = new BufferedReader(new FileReader(file), 8192)
    private val buf = new StringBuilder
    private val charBuf: Array[Char] = new Array(8192)
    private var bufPos, bufLen = 0
    private var nextToken: Option[String] = None
    private var eof = false

    private def fillBuffer(): Unit = {
      bufLen = reader.read(charBuf)
      bufPos = 0
      if (bufLen == -1) eof = true
    }

    private def nextChar(): Int = {
      if (bufPos >= bufLen) fillBuffer()
      if (eof) -1 else {
        val c = charBuf(bufPos)
        bufPos += 1
        c
      }
    }

    private var peekChar = nextChar()

    private def fetchNextToken(): Unit = {
      buf.clear()
      while (peekChar != -1 && peekChar.toChar.isWhitespace)
        peekChar = nextChar()
      while (peekChar != -1 && !peekChar.toChar.isWhitespace) {
        buf.append(peekChar.toChar)
        peekChar = nextChar()
      }
      nextToken = if (buf.nonEmpty) Some(buf.toString) else None
    }

    fetchNextToken()

    def hasNext: Boolean = nextToken.nonEmpty

    def next(): String = {
      val res = nextToken.getOrElse(throw new NoSuchElementException("next on empty iterator"))
      fetchNextToken()
      res
    }

    def head: String = nextToken.getOrElse(throw new NoSuchElementException("head on empty iterator"))

    def close(): Unit = reader.close()
  }

  def parse(filename: String, handlers: Map[String, Iterator[String] => Unit]): Map[String, SortedMap[Long, SignalVector]] = {

    val file = new java.io.File(filename)
    val tokens = whitespaceIterator(file)

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
      if (token.startsWith("$")) {
      } else if token.startsWith("#") then {
        current_parser_time = token.substring(1).toInt
      } else if (token.startsWith("b") || token.startsWith("B")) {
        val actual_num = token.substring(1)
        val identifier = tokens.next()
        if wires.contains(identifier) then {
          for (signalName, signalSize) <- wires(identifier) do {
            val signalVector = SignalVector(actual_num, signalSize)
            if !mut_signals.contains(signalName) then {
              mut_signals(signalName) = MutableSortedMap.empty
            }
            mut_signals(signalName)(current_parser_time) = signalVector
          }
          // val (signalName, signalSize) = wires(identifier)
        } else {
          //warning(s"Unknown identifier: $identifier")
        }
      } else if (token.startsWith("0") || token.startsWith("1") || token.startsWith("x") || token.startsWith("z") || 
              token.startsWith("X") || token.startsWith("Z")) {
        val value = token.charAt(0)
        val identifier = token.substring(1)
        if wires.contains(identifier) then {
          for (signalName, signalSize) <- wires(identifier) do {
            val signalVector = SignalVector(value.toString(), signalSize)
            if !mut_signals.contains(signalName) then {
              mut_signals(signalName) = MutableSortedMap.empty
            }
            mut_signals(signalName)(current_parser_time) = signalVector
          }
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
        println(s"Unknown scope type: $scopeType")
      } 
      current_scopes = scopeName :: current_scopes 
      //debug(s"Scope: $scopeType $scopeName")
    } else {
      println(s"Invalid scope definition: ${scopeDetails.mkString(" ")}")
    }
  }

  def handleUpScope(tokens: Iterator[String]): Unit = {
    if current_scopes.nonEmpty then {
      current_scopes = current_scopes.tail
    } else {
      println("No scopes to pop")
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
        if (wires.contains(varId)) {
          wires(varId) += (fullVarName, varSize)
        } else {
          wires(varId) = MutableSet((fullVarName, varSize))
        }

        if varType != "wire" && varType != "reg" then {
          println(s"Unknown variable type: $varType")
        }
      }
    } else {
      println(s"Invalid variable definition: ${varDetails.mkString(" ")}, SKIPPING")
    }
  }

  def exhaustIterator(tokens: Iterator[String]): Unit = {
    while (tokens.hasNext) {
      tokens.next()
    }
  }


  val wires: MutableMap[String, MutableSet[(String, Int)]] = MutableMap.empty

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
    current_parser_time
  }
}

import scala.collection.mutable
import scala.collection.immutable.SortedMap
import scala.io.Source
import scala.util.Using

class Gem5Parser(filename: String) extends ParsedGem5Data {
  def parseLogDataFast(filePath: String): Map[String, SortedMap[Long, Set[Long]]] = {
    // Use mutable maps and sets for high-performance, in-place accumulation
    val acc = mutable.Map.empty[String, mutable.Map[Long, mutable.Set[Long]]]

    val ret = Using(Source.fromFile(filePath)) { source =>
      for (line <- source.getLines()) {
        val len = line.length

        // 1. Find S (up to the first space)
        val sEnd = line.indexOf(' ')
        if (sEnd > 0) {
          val s = line.substring(0, sEnd)

          //if (s == "A" || s == "B" || s == "C") {

            // 2. Find T (skip any extra spaces between S and T)
            var tStart = sEnd + 1
            while (tStart < len && line.charAt(tStart) == ' ') tStart += 1

            if (tStart < len) {
              val tEnd = line.indexOf(' ', tStart)
              if (tEnd > tStart) {

                // 3. Find the Hex Value (skip extra spaces between T and Value)
                var vStart = tEnd + 1
                while (vStart < len && line.charAt(vStart) == ' ') vStart += 1

                if (vStart < len) {
                  // The value ends at the next space, or the end of the line
                  val vEnd = line.indexOf(' ', vStart)
                  val vEndActual = if (vEnd == -1) len else vEnd

                  try {
                    val tStr = line.substring(tStart, tEnd)
                    val vStrRaw = line.substring(vStart, vEndActual)

                    // Strip optional "0x" prefix if it happens to be there
                    val vStr = if (vStrRaw.startsWith("0x") || vStrRaw.startsWith("0X"))
                      vStrRaw.substring(2)
                    else
                      vStrRaw

                    val t = tStr.toLong / 1000
                    val v = java.lang.Long.parseLong(vStr, 16)

                    // Fast in-place accumulation
                    val sMap = acc.getOrElseUpdate(s, mutable.Map.empty)
                    val tSet = sMap.getOrElseUpdate(t, mutable.Set.empty)
                    tSet.add(v)
                  } catch {
                    case _: Exception => // Silently skip unparseable numbers (NumberFormatException)
                  }
                }
              }
            }
          //}
        }
      }
      acc // Return the mutable accumulator from the Using block
    }.map { mutableAcc =>
      // Convert the mutable structure to the requested immutable map structure
      mutableAcc.map { case (s, tMap) =>
        val immutableInnerMap = tMap.map { case (t, set) => (t, set.toSet) }.toMap
        (s, SortedMap.from(immutableInnerMap))
      }.toMap
    }.getOrElse(Map.empty) // Return empty Map if file fails to open
    current_parser_time = ret.values.flatMap(_.keys).maxOption.getOrElse(0L)
    ret
  }

  val signals = parseLogDataFast(filename)

  def getSignal(signalName: String): Option[SortedMap[Long, Set[Long]]] = {
    signals.get(signalName)
  }

  def getMaxTime: Long = {
    current_parser_time
  }
}