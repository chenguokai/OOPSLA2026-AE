package HT.observation

import HT.{TmpFiles, patchScriptPrefix}

import scala.collection.mutable.ArrayBuffer
import java.io.File
import scala.math.{max, min}

var GraphNodes = ArrayBuffer[ArrayBuffer[String]]()

var GraphWebNodes = ArrayBuffer[ArrayBuffer[EventNode]]()

var GraphEdges = ArrayBuffer[EdgeNode]()
var GraphWebEdges = ArrayBuffer[EdgeWebNode]()

var graphDOT: String = ""

def splitRowByOverlap(row: ArrayBuffer[EventNode]): ArrayBuffer[ArrayBuffer[EventNode]] = {
  val result = ArrayBuffer[ArrayBuffer[EventNode]]()

  for ((node, l) <- row.zipWithIndex) {
    var placed = false
    var i = 0

    // place to existing lines if possible
    for (line <- result if !placed) {
      val hasOverlap = line.exists(existing =>
        val ret = math.max(node.scycle, existing.scycle) <= min(node.ecycle, existing.ecycle)
        ret
      )
      if (!hasOverlap) {
        line += node.copy(name = node.name + s" ${l}", types = node.types + (if (i > 0) i.toString else "")) // suffix to distinguish same name nodes
        placed = true
      }
      i += 1
    }

    // if no proper lines, break into a new line
    if (!placed) {
      result += ArrayBuffer(node.copy(name = node.name + s" ${l}", types = node.types + (if (i > 0) i.toString else ""))) // suffix to distinguish same name nodes
    }
  }

  result
}

def splitRowByType(row: ArrayBuffer[EventNode]): ArrayBuffer[ArrayBuffer[EventNode]] = {
  val result = ArrayBuffer[ArrayBuffer[EventNode]]()

  for ((node, l) <- row.zipWithIndex) {
    var placed = false
    var i = 0

    // place to existing lines if possible
    for (line <- result if !placed) {
      if (line.head.types == node.types) {
        line += node
        placed = true
      }
      i += 1
    }

    // 如果没有合适的行，就新建一行
    if (!placed) {
      result += ArrayBuffer(node) // suffix to distinguish same name nodes
    }
  }

  result
}

def WebGraphPreprocess() = {
  GraphWebNodes = GraphWebNodes.flatMap { row =>
    splitRowByType(row)
  }
  /*
  GraphWebNodes = GraphWebNodes.flatMap { row =>
    splitRowByOverlap(row)
  }*/
}

def handleDotGraphNode(depth: Int, node: EventNode): Unit = {
  // depth: the tree depth
  if (node.children.isEmpty) {
    // leaf node
    while (GraphNodes.size <= depth) {
      GraphNodes += ArrayBuffer[String]()
    }
    GraphNodes(depth) += node.toString
  } else {
    for (child <- node.children) {
      // create an edge from parent to child
      handleDotGraphNode(depth + 1, child)
      GraphEdges += EdgeNode(depth, GraphNodes(depth).size, depth + 1, GraphNodes(depth+1).size - 1)
    }
    GraphNodes(depth) += node.toString
  }
}

var NameCounter: Map[String, Int] = Map()

def handleWebGraphNode(depth: Int, width: Int, node: EventNode): Unit = {
  // depth: the tree depth
  if (node.children.isEmpty) {
    // leaf node
    while (GraphWebNodes.size <= depth) {
      GraphWebNodes += ArrayBuffer[EventNode]()
    }
    GraphWebNodes(depth) += node
  } else {
    val hostCounter = NameCounter.get(node.name).get
    for ((child,i) <- node.children.zipWithIndex) {
      // create an edge from parent to child

      val childCounter = NameCounter.getOrElse(child.name, -1) + 1
      NameCounter += (child.name -> childCounter)
      GraphWebEdges += EdgeWebNode(node.name + s" ${hostCounter}", child.name + s" ${childCounter}")
      handleWebGraphNode(depth + 1, i, child)
    }
    GraphWebNodes(depth) += node
  }
}

def generateNodes(): String = {
  // generate the nodes in DOT format
  val ret = GraphNodes.zipWithIndex.map { case (nodes, depth) =>
    "{\nrank=same;\n" + nodes.map(node => s"""n${depth}_${nodes.indexOf(node)} [label="$node"];""").mkString("\n") + "}\n"
  }.mkString("\n")
  println("Generated Nodes:\n" + ret)
  ret
}

def generateEdges(): String = {
  // generate the edges in DOT format
  val ret = GraphEdges.map { edge =>
    s"""n${edge.fromDp}_${edge.fromIdx} -> n${edge.toDp}_${edge.toIdx};"""
  }.mkString("\n")
  println("Generated Edges:\n" + ret)
  ret
}

def generateDOT() = {
  // generate the DOT representation of the graph
  graphDOT =
    s"""
       |digraph EventGraph {
       |    rankdir=TB; // Top to Bottom layout. Use LR for Left to Right.
       |    node [shape=rectangle];
       |    ${generateNodes()}
       |    ${generateEdges()}
       |}
       |""".stripMargin
}

def generateWebNodes(): String = {
  // generate the nodes in web format
  GraphWebNodes.zipWithIndex.map { case (nodes, depth) =>
    nodes.zipWithIndex.map{ case (node, i) =>
      val head = node.name
      s"""${head}, ${node.types}, ${node.scycle}, ${node.ecycle}, ${head} from cycle ${node.scycle} to cycle ${node.ecycle} ${
        val s = if (node.startMeta != "") {
          s"start: ${node.startMeta}"
        } else {""}
        val e = if (node.endMeta != "") {
          s" end: ${node.endMeta}"
        } else {""}
        val b = if (node.breakMeta != "") {
          s" break: ${node.breakMeta}"
        } else {""}
        s + e + b
      }"""
    }.mkString("\n")
  }.mkString("\n")
}

def generateWebEdges(): String = {
  // generate the edges in web format
  GraphWebEdges.map { edge =>
    s"""${edge.from}, ${edge.to}"""
  }.mkString("\n")
}

val webColors = List("royalblue", "orange", "green", "red", "purple", "cyan", "blue", "yellow", "brown", "pink", "aqua", "gray")

def generateWebColors(): String = {
  // each depth gets a different color
  var ptr = 0
  GraphWebNodes.zipWithIndex.map { case (nodes, depth) =>
    val color = webColors(ptr % webColors.size)
    val name = nodes.head.name
    val nextName = if (depth + 1 < GraphWebNodes.size) GraphWebNodes(depth + 1).head.name else ""

    if (name.take(name.lastIndexOf(" ")) != nextName.take(nextName.lastIndexOf(" "))) {
      ptr += 1 // only increment if the name is the same, to avoid color change for each node
    }
    s"""${nodes.head.types}, ${color}"""
  }.mkString("\n")
}

def generateWeb(): String = {
  s"""
     |@
     |${generateWebNodes()}
     |@
     |${generateWebEdges()}
     |@
     |${generateWebColors()}
     |""".stripMargin
}

def generateGraph() = {

  // convert tree structure to graph with edges
  for ((n,i) <- GraphStack.stack.zipWithIndex) {
    val currentCounter = NameCounter.getOrElse(n.name, -1) + 1
    NameCounter += (n.name -> currentCounter) // initialize counter for each node
    handleWebGraphNode(0, i, n)
  }
  WebGraphPreprocess()

  // we have got layered nodes and edges
  // generateDOT() //deprecated, use generateWeb instead
  // println("Generated Graph DOT:\n" + graphDOT)

  val ret = generateWeb()
  // write ret to file
  val outFile = TmpFiles.figureFile.toString
  val writer = new java.io.PrintWriter(new File(outFile))
  writer.write(ret)
  writer.close()

  val htmlFile = TmpFiles.htmlFile.toString

  /*
  val compileHtml = patchScriptPrefix + "/visualization.py"
  val cmd = s"python3 $compileHtml $outFile $htmlFile"
  val plotResult = sys.process.Process(cmd).!!
  println("Starting HTTP Server at Port 8000")
  sys.process.Process(s"python3 -m http.server 8000 --directory ${TmpFiles.base_dir}").run()
   */
  ret
}