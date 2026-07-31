package HT.observation

import HT.StackOperations

sealed trait GraphNode

case class EventNode(name: String, types: String, scycle: Int, ecycle: Int, children: List[EventNode], startMeta: String, endMeta: String, breakMeta: String) extends GraphNode {
  override def toString: String = s"$name@$scycle"
}

case class EdgeNode(fromDp: Int, fromIdx: Int, toDp: Int, toIdx: Int) extends GraphNode

case class EdgeWebNode(from: String, to: String) extends GraphNode

val MAX_START_ATTEMPTS = 100000000
var GlobalCycle = 0

var GraphIteMin = 0
var GraphIteMax = 10000

def setGraphRange(min: Int, max: Int): Unit = {
  GraphIteMin = min
  GraphIteMax = max
}

object GraphStack extends StackOperations[EventNode]

object LogStack extends StackOperations[String]

/*
* Here we want to define onStart, onEnd, onBreak and inRange functions
* each range/until instance can have at most one instance of each function
* */

case class BlockNode(block: () => Unit)

object beforeStartStack extends StackOperations[BlockNode]
object onStartStack extends StackOperations[BlockNode]
object onEndStack extends StackOperations[BlockNode]
object onBreakStack extends StackOperations[BlockNode]
object inRangeStack extends StackOperations[BlockNode]

def beforeStart(block: => Unit): Unit = {
  // Push a new BlockNode onto the beforeStartStack
  beforeStartStack.push(BlockNode(() => block))
}

def entry(block: => Unit): Unit = {
  // Push a new BlockNode onto the onStartStack
  onStartStack.push(BlockNode(() => block))
}

def exit(block: => Unit): Unit = {
  // Push a new BlockNode onto the onEndStack
  onEndStack.push(BlockNode(() => block))
}

def abort(block: => Unit): Unit = {
  // Push a new BlockNode onto the onBreakStack
  onBreakStack.push(BlockNode(() => block))
}

def inRange(block: => Unit): Unit = {
  // Push a new BlockNode onto the inRangeStack
  inRangeStack.push(BlockNode(() => block))
}

def check(startCondition: () => Boolean, debug: String, startMeta: () => String = metaNothing)(block: => Unit): Unit = {
  // This function will execute the block only if the startCondition is true.
  // If the startCondition is false, it will not execute the block.
  val origGraphSp = GraphStack.size()
  if (startCondition()) {
    block
    val childrenNodes = GraphStack.slice(origGraphSp, GraphStack.size()).toList
    GraphStack.push(EventNode(debug, debug, GlobalCycle, GlobalCycle + 1, childrenNodes, startMeta = startMeta(), "", "")) // Push a dummy event node to the graph stack
  }
}

val metaNothing = () => ""

def getbsN() = {
  if (beforeStartStack.size() > 0) {
    // Execute the beforeStart block if it exists
    val r = beforeStartStack.top()
    beforeStartStack.pop(1)
    r
  } else {
    BlockNode(() => {})
  }
}

def getsN(origStartSp: Int) = {
  if (onStartStack.size() > origStartSp) {
    // Execute the onStart block if it exists
    val r = onStartStack.top()
    onStartStack.pop(1)
    r
  } else {
    BlockNode(() => {})
  }
}

def geteN(origEndSp: Int) = {
  if (onEndStack.size() > origEndSp) {
    // Execute the onEnd block if it exists
    val r = onEndStack.top()
    onEndStack.pop(1)
    r
  } else {
    BlockNode(() => {})
  }
}

def getiN(origInRangeSp: Int) = {
  if (inRangeStack.size() > origInRangeSp) {
    // Execute the inRange block if it exists
    val r = inRangeStack.top()
    inRangeStack.pop(1)
    r
  } else {
    BlockNode(() => {})
  }
}
def getbN(origBreakSp: Int) = {
  if (onBreakStack.size() > origBreakSp) {
    // Execute the onBreak block if it exists
    val r = onBreakStack.top()
    onBreakStack.pop(1)
    r
  } else {
    BlockNode(() => {})
  }
}

def Range(startCondition: () => Boolean, stopCondition: () => Boolean, debug: String, startMeta: () => String = metaNothing, endMeta: () => String = metaNothing, maxCycle: Long = current_parser_time)(block: => Unit): Unit = {
  // --- Phase 1: Wait for the start condition ---
  // This loop continues until `startCondition` returns true or the maxCycle are used up.
  // println(s"Waiting for start condition (max attempts: $maxCycle)...")
  val origCycle = GlobalCycle
  var origGraphSp = GraphStack.size()

  var startStr = ""
  var endStr = ""

  val originBeforeStartSp = beforeStartStack.size()
  val origStartSp = onStartStack.size()
  val origEndSp = onEndStack.size()
  val origBreakSp = onBreakStack.size()
  val origInRangeSp = inRangeStack.size()

  block // grab the nodes

  assert(beforeStartStack.size() - originBeforeStartSp <= 1, "There should be at most one beforeStart block defined for each range/until instance.")
  assert(onStartStack.size() - origStartSp <= 1, "There should be at most one onStart block defined for each range/until instance.")
  assert(onEndStack.size() - origEndSp <= 1, "There should be at most one onEnd block defined for each range/until instance.")
  assert(onBreakStack.size() == origBreakSp, "There should be no onBreak block defined for each range/until instance.")
  assert(inRangeStack.size() - origInRangeSp <= 1, "There should be at most one inRange block defined for each range/until instance.")

  val bsN = getbsN()
  val sN = getsN(origStartSp)
  val eN = geteN(origEndSp)
  val iN = getiN(origInRangeSp)

  while (GlobalCycle < maxCycle) {
    // println(s"Iteration $GlobalIte")
    if (startCondition()) {
      val logSp = LogStack.size()
      sN.block()
      val logSlice = LogStack.slice(logSp, LogStack.size()).reduceOption(_ + " " + _).getOrElse("")
      startStr = logSlice // startMeta()
      LogStack.pop(LogStack.size() - logSp)
      println("start: " + startStr)
      var shouldContinue = true
      val GlobalBackup = GlobalCycle
      while (shouldContinue) {
        // Execute the lazily-evaluated code block.
        iN.block()
        GlobalCycle += 1
        // After execution, check the stop condition.
        if (stopCondition()) {
          val logSpE = LogStack.size()
          eN.block()
          val logSliceE = LogStack.slice(logSpE, LogStack.size()).reduceOption(_ + " " + _).getOrElse("")
          endStr = logSliceE // endMeta()
          LogStack.pop(LogStack.size() - logSpE)
          println(s"⏹️ Stop condition met. Terminating loop. $endStr")
          shouldContinue = false // Terminate the execution loop.
        } else if (GlobalCycle >= maxCycle) {
          shouldContinue = false
          println(s"⏹️ MAX condition met. Terminating loop. $debug")
        }
      }
      val childrenNodes = GraphStack.slice(origGraphSp, GraphStack.size()).toList
      GraphStack.pop(GraphStack.size() - origGraphSp)
      if (GlobalCycle >= GraphIteMin && GlobalCycle <= GraphIteMax) {
        GraphStack.push(EventNode(debug, debug, GlobalBackup, GlobalCycle, childrenNodes, startStr, endStr, ""))
        origGraphSp += 1
      }
      GlobalCycle = GlobalBackup
    } else {
      bsN.block() // Execute the beforeStart block if it exists
    }
    GlobalCycle += 1
  }
  GlobalCycle = origCycle // Reset GlobalIte to its original value after the loop.
}

def rangeNext(startCondition: () => Boolean, stopCondition: () => Boolean, debug: String, startMeta: () => String = metaNothing, endMeta: () => String = metaNothing, maxCycle: Long = current_parser_time)(block: => Unit): Unit = {
  // --- Phase 1: Wait for the start condition ---
  // This loop continues until `startCondition` returns true or the maxCycle are used up.
  // println(s"Waiting for start condition (max attempts: $maxCycle)...")
  val origCycle = GlobalCycle
  var origGraphSp = GraphStack.size()
  var startStr = ""
  var endStr = ""

  val originBeforeStartSp = beforeStartStack.size()
  val origStartSp = onStartStack.size()
  val origEndSp = onEndStack.size()
  val origBreakSp = onBreakStack.size()
  val origInRangeSp = inRangeStack.size()

  block // grab the nodes

  assert(beforeStartStack.size() - originBeforeStartSp <= 1, "There should be at most one beforeStart block defined for each range/untilNext instance.")
  assert(onStartStack.size() - origStartSp <= 1, "There should be at most one onStart block defined for each range/untilNext instance.")
  assert(onEndStack.size() - origEndSp <= 1, "There should be at most one onEnd block defined for each range/untilNext instance.")
  assert(onBreakStack.size() == origBreakSp, "There should be no onBreak block defined for each range/untilNext instance.")
  assert(inRangeStack.size() - origInRangeSp <= 1, "There should be at most one inRange block defined for each range/untilNext instance.")

  val bsN = getbsN()
  val sN = getsN(origStartSp)
  val eN = geteN(origEndSp)
  val iN = getiN(origInRangeSp)

  while (GlobalCycle < maxCycle) {
    // println(s"Iteration $GlobalIte")
    if (startCondition()) {
      val logSp = LogStack.size()
      sN.block()
      val logSlice = LogStack.slice(logSp, LogStack.size()).reduceOption(_ + " " + _).getOrElse("")
      startStr = logSlice // startMeta()
      LogStack.pop(LogStack.size() - logSp)
      println("start: " + startStr)
      var shouldContinue = true
      val GlobalBackup = GlobalCycle
      GlobalCycle += 1
      while (shouldContinue) {
        // Execute the lazily-evaluated code block.
        iN.block()
        GlobalCycle += 1
        // After execution, check the stop condition.
        if (stopCondition()) {
          val logSpE = LogStack.size()
          eN.block()
          val logSliceE = LogStack.slice(logSpE, LogStack.size()).reduceOption(_ + " " + _).getOrElse("")
          endStr = logSliceE // endMeta()
          LogStack.pop(LogStack.size() - logSpE)
          println(s"⏹️ Stop condition met. Terminating loop. $endStr")
          shouldContinue = false // Terminate the execution loop.
        } else if (GlobalCycle >= maxCycle) {
          shouldContinue = false
          println(s"⏹️ MAX condition met. Terminating loop. $debug")
        }
      }

      val childrenNodes = GraphStack.slice(origGraphSp, GraphStack.size()).toList
      GraphStack.pop(GraphStack.size() - origGraphSp) // Clear the stack after pushing the new node
      // build graph node
      if (GlobalCycle >= GraphIteMin && GlobalCycle <= GraphIteMax) {
        GraphStack.push(EventNode(debug, debug, GlobalBackup, GlobalCycle, childrenNodes, startStr, endStr, ""))
        origGraphSp += 1
      }
      GlobalCycle = GlobalBackup
    } else {
      bsN.block() // Execute the beforeStart block if it exists
    }
    GlobalCycle += 1
  }
  GlobalCycle = origCycle // Reset GlobalIte to its original value after the loop.
}

def rangeNextOnce(startCondition: () => Boolean, stopCondition: () => Boolean, debug: String, startMeta: () => String = metaNothing, endMeta: () => String = metaNothing, maxCycle: Long = current_parser_time)(block: => Unit): Unit = {
  // --- Phase 1: Wait for the start condition ---
  // This loop continues until `startCondition` returns true or the maxCycle are used up.
  // println(s"Waiting for start condition (max attempts: $maxCycle)...")
  var shouldContinue = true
  val origCycle = GlobalCycle
  var origGraphSp = GraphStack.size()
  var startStr = ""
  var endStr = ""

  val originBeforeStartSp = beforeStartStack.size()
  val origStartSp = onStartStack.size()
  val origEndSp = onEndStack.size()
  val origBreakSp = onBreakStack.size()
  val origInRangeSp = inRangeStack.size()
  block // grab the nodes

  assert(beforeStartStack.size() - originBeforeStartSp <= 1, "There should be at most one beforeStart block defined for each range/untilNextOnce instance.")
  assert(onStartStack.size() - origStartSp <= 1, "There should be at most one onStart block defined for each range/untilNextOnce instance.")
  assert(onEndStack.size() - origEndSp <= 1, "There should be at most one onEnd block defined for each range/untilNextOnce instance.")
  assert(onBreakStack.size() == origBreakSp, "There should be no onBreak block defined for each range/untilNextOnce instance.")
  assert(inRangeStack.size() - origInRangeSp <= 1, "There should be at most one inRange block defined for each range/untilNextOnce instance.")

  val bsN = getbsN()
  val sN = getsN(origStartSp)
  val eN = geteN(origEndSp)
  val iN = getiN(origInRangeSp)

  while (GlobalCycle < maxCycle && shouldContinue) {
    if (startCondition()) {
      val logSp = LogStack.size()
      sN.block()
      val logSlice = LogStack.slice(logSp, LogStack.size()).reduceOption(_ + " " + _).getOrElse("")
      startStr = logSlice // startMeta()
      LogStack.pop(LogStack.size() - logSp)
      println("start: " + startStr)
      val GlobalBackup = GlobalCycle
      GlobalCycle += 1
      while (shouldContinue) {
        // Execute the lazily-evaluated code block.
        iN.block()
        GlobalCycle += 1
        // After execution, check the stop condition.
        if (stopCondition()) {
          val logSpE = LogStack.size()
          eN.block()
          val logSliceE = LogStack.slice(logSpE, LogStack.size()).reduceOption(_ + " " + _).getOrElse("")
          endStr = logSliceE // endMeta()
          LogStack.pop(LogStack.size() - logSpE)
          println(s"⏹️ Stop condition met. Terminating loop.")
          shouldContinue = false // Terminate the execution loop.
        } else if (GlobalCycle >= maxCycle) {
          shouldContinue = false
          println(s"⏹️ MAX condition met. Terminating loop. $debug")
        }
      }
      val childrenNodes = GraphStack.slice(origGraphSp, GraphStack.size()).toList
      GraphStack.pop(GraphStack.size() - origGraphSp)
      if (GlobalCycle >= GraphIteMin && GlobalCycle <= GraphIteMax) {
        GraphStack.push(EventNode(debug, debug, GlobalBackup, GlobalCycle, childrenNodes, startStr, endStr, ""))
        origGraphSp += 1
      }
      GlobalCycle = GlobalBackup
    } else {
      bsN.block() // Execute the beforeStart block if it exists
    }
    GlobalCycle += 1
  }
  GlobalCycle = origCycle // Reset GlobalIte to its original value after the loop.
}

def rangeOnce(startCondition: () => Boolean, stopCondition: () => Boolean, debug: String, startMeta: () => String = metaNothing, endMeta: () => String = metaNothing, maxCycle: Long = current_parser_time)(block: => Unit): Unit = {
  // --- Phase 1: Wait for the start condition ---
  // This loop continues until `startCondition` returns true or the maxCycle are used up.
  // println(s"Waiting for start condition (max attempts: $maxCycle)...")
  var shouldContinue = true
  val origCycle = GlobalCycle
  var origGraphSp = GraphStack.size()
  var startStr = ""
  var endStr = ""

  val originBeforeStartSp = beforeStartStack.size()
  val origStartSp = onStartStack.size()
  val origEndSp = onEndStack.size()
  val origBreakSp = onBreakStack.size()
  val origInRangeSp = inRangeStack.size()
  block // grab the nodes

  assert(beforeStartStack.size() - originBeforeStartSp <= 1, "There should be at most one beforeStart block defined for each range/untilOnce instance.")
  assert(onStartStack.size() - origStartSp <= 1, "There should be at most one onStart block defined for each range/untilOnce instance.")
  assert(onEndStack.size() - origEndSp <= 1, "There should be at most one onEnd block defined for each range/untilOnce instance.")
  assert(onBreakStack.size() == origBreakSp, "There should be no onBreak block defined for each range/untilOnce instance.")
  assert(inRangeStack.size() - origInRangeSp <= 1, "There should be at most one inRange block defined for each range/untilOnce instance.")
  val bsN = getbsN()
  val sN = getsN(origStartSp)
  val eN = geteN(origEndSp)
  val iN = getiN(origInRangeSp)

  while (GlobalCycle < maxCycle && shouldContinue) {
    if (startCondition()) {
      val logSp = LogStack.size()
      sN.block()
      val logSlice = LogStack.slice(logSp, LogStack.size()).reduceOption(_ + " " + _).getOrElse("")
      startStr = logSlice// startMeta()
      LogStack.pop(LogStack.size() - logSp)
      println("start: " + startStr)
      val GlobalBackup = GlobalCycle
      while (shouldContinue) {
        // Execute the lazily-evaluated code block.
        iN.block()
        GlobalCycle += 1
        // After execution, check the stop condition.
        if (stopCondition()) {
          val logSpE = LogStack.size()
          eN.block()
          val logSliceE = LogStack.slice(logSpE, LogStack.size()).reduceOption(_ + " " + _).getOrElse("")
          endStr = logSliceE// endMeta()
          LogStack.pop(LogStack.size() - logSpE)
          println(s"⏹️ Stop condition met. Terminating loop. $endStr")
          shouldContinue = false // Terminate the execution loop.
        } else if (GlobalCycle >= maxCycle) {
          shouldContinue = false
          println(s"⏹️ MAX condition met. Terminating loop. $debug")
        }
      }
      val childrenNodes = GraphStack.slice(origGraphSp, GraphStack.size()).toList
      GraphStack.pop(GraphStack.size() - origGraphSp)
      if (GlobalCycle >= GraphIteMin && GlobalCycle <= GraphIteMax) {
        GraphStack.push(EventNode(debug, debug, GlobalBackup, GlobalCycle, childrenNodes, startStr, endStr, ""))
        origGraphSp += 1
      }
      GlobalCycle = GlobalBackup
    } else {
      bsN.block() // Execute the beforeStart block if it exists
      GlobalCycle += 1
    }
  }
  GlobalCycle = origCycle // Reset GlobalIte to its original value after the loop.
}

def rangeUnless(startCondition: () => Boolean, stopCondition: () => Boolean, breakCondition: () => Boolean, debug: String, startMeta: () => String = metaNothing, endMeta: () => String = metaNothing, breakMeta: () => String = metaNothing, maxCycle: Long = current_parser_time)(block: => Unit): Unit = {
  // --- Phase 1: Wait for the start condition ---
  // This loop continues until `startCondition` returns true or the maxCycle are used up.
  // println(s"Waiting for start condition (max attempts: $maxCycle)...")
  var shouldBreak = false
  val origCycle = GlobalCycle
  var origGraphSp = GraphStack.size()
  var startStr = ""
  var endStr = ""
  var breakStr = ""

  val originBeforeStartSp = beforeStartStack.size()
  val origStartSp = onStartStack.size()
  val origEndSp = onEndStack.size()
  val origBreakSp = onBreakStack.size()
  val origInRangeSp = inRangeStack.size()
  block // grab the nodes

  assert(beforeStartStack.size() - originBeforeStartSp <= 1, "There should be at most one beforeStart block defined for each range/untilWithBreak instance.")
  assert(onStartStack.size() - origStartSp <= 1, "There should be at most one onStart block defined for each range/untilWithBreak instance.")
  assert(onEndStack.size() - origEndSp <= 1, "There should be at most one onEnd block defined for each range/untilWithBreak instance.")
  assert(onBreakStack.size() - origBreakSp <= 1, "There should be at most one onBreak block defined for each range/untilWithBreak instance.")
  assert(inRangeStack.size() - origInRangeSp <= 1, "There should be at most one inRange block defined for each range/untilWithBreak instance.")
  val bsN = getbsN()
  val sN = getsN(origStartSp)
  val eN = geteN(origEndSp)
  val iN = getiN(origInRangeSp)
  val bN = getbN(origBreakSp)

  while (GlobalCycle < maxCycle && !shouldBreak) {
    if (startCondition()) {
      val logSp = LogStack.size()
      sN.block()
      val logSlice = LogStack.slice(logSp, LogStack.size()).reduceOption(_ + " " + _).getOrElse("")
      startStr = logSlice // startMeta()
      LogStack.pop(LogStack.size() - logSp)
      // println("start: " + startStr)
      var shouldContinue = true
      val GlobalBackup = GlobalCycle
      GlobalCycle += 1
      while (shouldContinue) {
        // Execute the lazily-evaluated code block.
        iN.block()

        GlobalCycle += 1

        // After execution, check the stop condition.
        if (stopCondition()) {
          val logSpE = LogStack.size()
          eN.block()
          val logSliceE = LogStack.slice(logSpE, LogStack.size()).reduceOption(_ + " " + _).getOrElse("")
          endStr = logSliceE // endMeta()
          LogStack.pop(LogStack.size() - logSpE)
          // println(s"⏹️ Stop condition met. Terminating loop. $endStr")
          shouldContinue = false // Terminate the execution loop.
        } else if (GlobalCycle >= maxCycle) {
          shouldContinue = false
          println(s"⏹️ MAX condition met. Terminating loop. $debug")
        }
      }

      val childrenNodes = GraphStack.slice(origGraphSp, GraphStack.size()).toList
      GraphStack.pop(GraphStack.size() - origGraphSp) // Clear the stack after pushing the new node
      // build graph node
      if (GlobalCycle >= GraphIteMin && GlobalCycle <= GraphIteMax) {
        GraphStack.push(EventNode(debug, debug, GlobalBackup, GlobalCycle, childrenNodes, startStr, endStr, ""))
        origGraphSp += 1
      }
      GlobalCycle = GlobalBackup
    } else {
      bsN.block() // Execute the beforeStart block if it exists
    }
    if (breakCondition()) {
      val logSpB = LogStack.size()
      bN.block()
      val logSliceB = LogStack.slice(logSpB, LogStack.size()).reduceOption(_ + " " + _).getOrElse("")
      breakStr = logSliceB // breakMeta()
      LogStack.pop(LogStack.size() - logSpB)
      println(s"break: " + breakStr)
      shouldBreak = true
      println(s"⏹️ Break condition met. Terminating loop. $debug at cycle $GlobalCycle")
    }
    GlobalCycle += 1
  }
  GlobalCycle = origCycle // Reset GlobalIte to its original value after the loop.
}

def rangeNextOnceUnless(startCondition: () => Boolean, stopCondition: () => Boolean, breakCondition: () => Boolean, debug: String, startMeta: () => String = metaNothing, endMeta: () => String = metaNothing, breakMeta: () => String = metaNothing, maxCycle: Long = current_parser_time)(block: => Unit): Unit = {
  // --- Phase 1: Wait for the start condition ---
  // This loop continues until `startCondition` returns true or the maxCycle are used up.
  // println(s"Waiting for start condition (max attempts: $maxCycle)...")
  var shouldContinue = true
  val origCycle = GlobalCycle
  var startStr = ""
  var endStr = ""
  var breakStr = ""

  val originBeforeStartSp = beforeStartStack.size()
  val origStartSp = onStartStack.size()
  val origEndSp = onEndStack.size()
  val origBreakSp = onBreakStack.size()
  val origInRangeSp = inRangeStack.size()
  block // grab the nodes
  assert(beforeStartStack.size() - originBeforeStartSp <= 1, "There should be at most one beforeStart block defined for each range/untilNextOnceWithBreak instance.")
  assert(onStartStack.size() - origStartSp <= 1, "There should be at most one onStart block defined for each range/untilNextOnceWithBreak instance.")
  assert(onEndStack.size() - origEndSp <= 1, "There should be at most one onEnd block defined for each range/untilNextOnceWithBreak instance.")
  assert(onBreakStack.size() - origBreakSp <= 1, "There should be at most one onBreak block defined for each range/untilNextOnceWithBreak instance.")
  assert(inRangeStack.size() - origInRangeSp <= 1, "There should be at most one inRange block defined for each range/untilNextOnceWithBreak instance.")
  val bsN = getbsN()
  val sN = getsN(origStartSp)
  val eN = geteN(origEndSp)
  val iN = getiN(origInRangeSp)
  val bN = getbN(origBreakSp)

  var origGraphSp = GraphStack.size()
  while (GlobalCycle < maxCycle && shouldContinue) {
    if (startCondition()) {
      val logSp = LogStack.size()
      sN.block()
      val logSlice = LogStack.slice(logSp, LogStack.size()).reduceOption(_ + " " + _).getOrElse("")
      startStr = logSlice // startMeta()
      LogStack.pop(LogStack.size() - logSp)
      println("start: " + startStr)
      val GlobalBackup = GlobalCycle
      GlobalCycle += 1
      while (shouldContinue) {
        // Execute the lazily-evaluated code block.
        iN.block()

        GlobalCycle += 1

        // After execution, check the stop condition.
        if (stopCondition()) {
          val logSpE = LogStack.size()
          eN.block()
          val logSliceE = LogStack.slice(logSpE, LogStack.size()).reduceOption(_ + " " + _).getOrElse("")
          endStr = logSliceE // endMeta()
          LogStack.pop(LogStack.size() - logSpE)
          println(s"⏹️ Stop condition met. Terminating loop. $endStr")
          shouldContinue = false // Terminate the execution loop.
        } else if (GlobalCycle >= maxCycle) {
          shouldContinue = false
          println(s"⏹️ MAX condition met. Terminating loop. $debug")
        }
      }

      val childrenNodes = GraphStack.slice(origGraphSp, GraphStack.size()).toList
      GraphStack.pop(GraphStack.size() - origGraphSp) // Clear the stack after pushing the new node
      // build graph node
      if (GlobalCycle >= GraphIteMin && GlobalCycle <= GraphIteMax) {
        GraphStack.push(EventNode(debug, debug, GlobalBackup, GlobalCycle, childrenNodes, startStr, endStr, ""))
        origGraphSp += 1
      }
      GlobalCycle = GlobalBackup
    } else {
      bsN.block() // Execute the beforeStart block if it exists
    }
    if (breakCondition()) {
      val logSpB = LogStack.size()
      bN.block()
      val logSliceB = LogStack.slice(logSpB, LogStack.size()).reduceOption(_ + " " + _).getOrElse("")
      breakStr = logSliceB// breakMeta()
      LogStack.pop(LogStack.size() - logSpB)
      println(s"break: " + breakStr)
      shouldContinue = false
      println(s"⏹️ Break condition met. Terminating loop. $debug at cycle $GlobalCycle")
    }
    GlobalCycle += 1
  }
  GlobalCycle = origCycle // Reset GlobalIte to its original value after the loop.
}

def rangeOnceUnless(startCondition: () => Boolean, stopCondition: () => Boolean, breakCondition: () => Boolean, debug: String, startMeta: () => String = metaNothing, endMeta: () => String = metaNothing, breakMeta: () => String = metaNothing, maxCycle: Long = current_parser_time)(block: => Unit): Unit = {
  // --- Phase 1: Wait for the start condition ---
  // This loop continues until `startCondition` returns true or the maxCycle are used up.
  // println(s"Waiting for start condition (max attempts: $maxCycle)...")
  var shouldContinue = true
  val origCycle = GlobalCycle
  var origGraphSp = GraphStack.size()
  var startStr = ""
  var endStr = ""
  var breakStr = ""

  val originBeforeStartSp = beforeStartStack.size()
  val origStartSp = onStartStack.size()
  val origEndSp = onEndStack.size()
  val origBreakSp = onBreakStack.size()
  val origInRangeSp = inRangeStack.size()
  block // grab the nodes
  assert(beforeStartStack.size() - originBeforeStartSp <= 1, "There should be at most one beforeStart block defined for each range/untilOnceWithBreak instance.")
  assert(onStartStack.size() - origStartSp <= 1, "There should be at most one onStart block defined for each range/untilOnceWithBreak instance.")
  assert(onEndStack.size() - origEndSp <= 1, "There should be at most one onEnd block defined for each range/untilOnceWithBreak instance.")
  assert(onBreakStack.size() - origBreakSp <= 1, "There should be at most one onBreak block defined for each range/untilOnceWithBreak instance.")
  assert(inRangeStack.size() - origInRangeSp <= 1, "There should be at most one inRange block defined for each range/untilOnceWithBreak instance.")
  val bsN = getbsN()
  val sN = getsN(origStartSp)
  val eN = geteN(origEndSp)
  val iN = getiN(origInRangeSp)
  val bN = getbN(origBreakSp)

  while (GlobalCycle < maxCycle && shouldContinue) {
    if (startCondition()) {
      val logSp = LogStack.size()
      sN.block()
      val logSlice = LogStack.slice(logSp, LogStack.size()).reduceOption(_ + " " + _).getOrElse("")
      startStr = logSlice // startMeta()
      LogStack.pop(LogStack.size() - logSp)
      println("start: " + startStr)
      val GlobalBackup = GlobalCycle
      while (shouldContinue) {
        // Execute the lazily-evaluated code block.
        iN.block()

        GlobalCycle += 1

        // After execution, check the stop condition.
        if (stopCondition()) {
          val logSpE = LogStack.size()
          eN.block()
          val logSliceE = LogStack.slice(logSpE, LogStack.size()).reduceOption(_ + " " + _).getOrElse("")
          endStr = logSliceE // endMeta()
          LogStack.pop(LogStack.size() - logSpE)
          println(s"⏹️ Stop condition met. Terminating loop. $endStr")
          shouldContinue = false // Terminate the execution loop.
        } else if (GlobalCycle >= maxCycle) {
          shouldContinue = false
          println(s"⏹️ MAX condition met. Terminating loop. $debug")
        }
      }

      val childrenNodes = GraphStack.slice(origGraphSp, GraphStack.size()).toList
      GraphStack.pop(GraphStack.size() - origGraphSp) // Clear the stack after pushing the new node
      // build graph node
      if (GlobalCycle >= GraphIteMin && GlobalCycle <= GraphIteMax) {
        GraphStack.push(EventNode(debug, debug, GlobalBackup, GlobalCycle, childrenNodes, startStr, endStr, ""))
        origGraphSp += 1
      }
      GlobalCycle = GlobalBackup
    } else {
      bsN.block() // Execute the beforeStart block if it exists
    }
    if (breakCondition()) {
      val logSpB = LogStack.size()
      bN.block()
      val logSliceB = LogStack.slice(logSpB, LogStack.size()).reduceOption(_ + " " + _).getOrElse("")
      breakStr = logSliceB // breakMeta()
      LogStack.pop(LogStack.size() - logSpB)
      println(s"break: " + breakStr)
      shouldContinue = false
      println(s"⏹️ Break condition met. Terminating loop. $debug at cycle $GlobalCycle")
    }
    GlobalCycle += 1
  }
  GlobalCycle = origCycle // Reset GlobalIte to its original value after the loop.
}

def rangeNextUnless(startCondition: () => Boolean, stopCondition: () => Boolean, breakCondition: () => Boolean, debug: String, startMeta: () => String = metaNothing, endMeta: () => String = metaNothing, breakMeta: () => String = metaNothing, maxCycle: Long = current_parser_time)(block: => Unit): Unit = {
  // --- Phase 1: Wait for the start condition ---
  // This loop continues until `startCondition` returns true or the maxCycle are used up.
  // println(s"Waiting for start condition (max attempts: $maxCycle)...")
  var shouldBreak = false
  val origCycle = GlobalCycle
  var origGraphSp = GraphStack.size()
  var startStr = ""
  var endStr = ""
  var breakStr = ""

  val originBeforeStartSp = beforeStartStack.size()
  val origStartSp = onStartStack.size()
  val origEndSp = onEndStack.size()
  val origBreakSp = onBreakStack.size()
  val origInRangeSp = inRangeStack.size()
  block // grab the nodes
  assert(beforeStartStack.size() - originBeforeStartSp <= 1, "There should be at most one beforeStart block defined for each range/untilNextWithBreak instance.")
  assert(onStartStack.size() - origStartSp <= 1, "There should be at most one onStart block defined for each range/untilNextWithBreak instance.")
  assert(onEndStack.size() - origEndSp <= 1, "There should be at most one onEnd block defined for each range/untilNextWithBreak instance.")
  assert(onBreakStack.size() - origBreakSp <= 1, "There should be at most one onBreak block defined for each range/untilNextWithBreak instance.")
  assert(inRangeStack.size() - origInRangeSp <= 1, "There should be at most one inRange block defined for each range/untilNextWithBreak instance.")
  val bsN = getbsN()
  val sN = getsN(origStartSp)
  val eN = geteN(origEndSp)
  val iN = getiN(origInRangeSp)
  val bN = getbN(origBreakSp)

  while (GlobalCycle < maxCycle && !shouldBreak) {
    if (startCondition()) {
      val logSp = LogStack.size()
      sN.block()
      val logSlice = LogStack.slice(logSp, LogStack.size()).reduceOption(_ + " " + _).getOrElse("")
      startStr = logSlice // startMeta()
      LogStack.pop(LogStack.size() - logSp)
      println("start: " + startStr)
      var shouldContinue = true
      val GlobalBackup = GlobalCycle
      GlobalCycle += 1
      while (shouldContinue) {
        // Execute the lazily-evaluated code block.
        iN.block()

        GlobalCycle += 1

        // After execution, check the stop condition.
        if (stopCondition()) {
          val logSpE = LogStack.size()
          eN.block()
          val logSliceE = LogStack.slice(logSpE, LogStack.size()).reduceOption(_ + " " + _).getOrElse("")
          endStr = logSliceE // endMeta()
          LogStack.pop(LogStack.size() - logSpE)
          println(s"⏹️ Stop condition met. Terminating loop. $endStr")
          shouldContinue = false // Terminate the execution loop.
        } else if (GlobalCycle >= maxCycle) {
          shouldContinue = false
          println(s"⏹️ MAX condition met. Terminating loop. $debug")
        }
      }

      val childrenNodes = GraphStack.slice(origGraphSp, GraphStack.size()).toList
      GraphStack.pop(GraphStack.size() - origGraphSp) // Clear the stack after pushing the new node
      // build graph node
      if (GlobalCycle >= GraphIteMin && GlobalCycle <= GraphIteMax) {
        GraphStack.push(EventNode(debug, debug, GlobalBackup, GlobalCycle, childrenNodes, startStr, endStr, ""))
        origGraphSp += 1
      }
      GlobalCycle = GlobalBackup
    } else {
      bsN.block() // Execute the beforeStart block if it exists
    }
    if (breakCondition()) {
      val logSpB = LogStack.size()
      bN.block()
      val logSliceB = LogStack.slice(logSpB, LogStack.size()).reduceOption(_ + " " + _).getOrElse("")
      breakStr = logSliceB // breakMeta()
      LogStack.pop(LogStack.size() - logSpB)
      println(s"break: " + breakStr)
      shouldBreak = true
      println(s"⏹️ Break condition met. Terminating loop. $debug at cycle $GlobalCycle")
    }
    GlobalCycle += 1
  }
  GlobalCycle = origCycle // Reset GlobalIte to its original value after the loop.
}

val untilCond = () => {true}

def On(startCondition: () => Boolean, debug: String, startMeta: () => String = metaNothing, maxCycle: Long = current_parser_time)(block: => Unit): Unit = {
  Range(startCondition, untilCond, debug, startMeta, maxCycle = maxCycle) {
    block
  }
}

def OnUnless(startCondition: () => Boolean, breakCondition: () => Boolean, debug: String, startMeta: () => String = metaNothing, breakMeta: () => String = metaNothing, maxCycle: Long = current_parser_time)(block: => Unit): Unit = {
  rangeUnless(startCondition, untilCond, breakCondition, debug, startMeta, breakMeta = breakMeta, maxCycle = maxCycle) {
    block
  }
}

def OnNextUnless(startCondition: () => Boolean, breakCondition: () => Boolean, debug: String, startMeta: () => String = metaNothing, breakMeta: () => String = metaNothing, maxCycle: Long = current_parser_time)(block: => Unit): Unit = {
  rangeNextUnless(startCondition, untilCond, breakCondition, debug, startMeta, breakMeta = breakMeta, maxCycle = maxCycle) {
    block
  }
}

def OnNextOnceUnless(startCondition: () => Boolean, breakCondition: () => Boolean, debug: String, startMeta: () => String = metaNothing, breakMeta: () => String = metaNothing, maxCycle: Long = current_parser_time)(block: => Unit): Unit = {
  rangeNextOnceUnless(startCondition, untilCond, breakCondition, debug, startMeta, breakMeta = breakMeta, maxCycle = maxCycle) {
    block
  }
}

def OnOnceUnless(startCondition: () => Boolean, breakCondition: () => Boolean, debug: String, startMeta: () => String = metaNothing, breakMeta: () => String = metaNothing, maxCycle: Long = current_parser_time)(block: => Unit): Unit = {
  rangeOnceUnless(startCondition, untilCond, breakCondition, debug, startMeta, breakMeta = breakMeta, maxCycle = maxCycle) {
    block
  }
}

def OnNextOnce(startCondition: () => Boolean, debug: String, startMeta: () => String = metaNothing, maxCycle: Long = current_parser_time)(block: => Unit): Unit = {
  rangeNextOnce(startCondition, untilCond, debug, startMeta, maxCycle = maxCycle) {
    block
  }
}

def EventLog(s: String) = {
  println(s)
  LogStack.push(s)
}