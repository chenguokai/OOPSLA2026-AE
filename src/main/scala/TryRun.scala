package HT

// In this file, we try to compile the code and extract offsets for interesting labels

import HT.{AdvancedRelocation, FuncDeclCode, RemoteCode, SMTCode, branchMapTryRun}
import HT.CodeGen.{LinkerPostfix, LinkerPrefix, LinkerTryRun}

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import HT.*
import HT.AsmPatcher.AsmInserter
import HT.Permissions.CurrentDefaultPermission
import HT.Permissions.Permission.VictimPrivate

// store interesting labels from victim world
object InterestingLabels {
  var Labels = ListBuffer[String]()
  def insert(label: String): Unit = {
    if (CurrentDefaultPermission != VictimPrivate) {
      ControlAttackerInterestingLabels.insert(label)
    } else {
      Labels += label
    }
  }
  def merge(): Unit = {
    Labels ++= ControlAttackerInterestingLabels.Labels
  }
}

// store interesting labels from control and attacker world, can be cleared when required
object ControlAttackerInterestingLabels {
  var Labels = ListBuffer[String]()
  def insert(label: String): Unit = {
    Labels += label
  }
  def clear(): Unit = {
    Labels.clear()
  }
}

case class SymbolEntry(
                        num: Int,
                        value: String,
                        size: String,
                        entryType: String,
                        bind: String,
                        visibility: String,
                        ndx: String,
                        name: Option[String]
                      )

object SymbolTableParser {
  // Regex pattern for symbol table entries
  private val entryPattern: Regex = """\s*(\d+):\s+([0-9a-f]+)\s+(0x[0-9a-fA-F]+|\d+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s*(.*)""".r

  def parse(input: String): Seq[SymbolEntry] = {
    val entries = input
      .split("\n")
      .flatMap {
        case entryPattern(num, value, size, entryType, bind, visibility, ndx, name) =>
          Some(
            SymbolEntry(
              num.toInt,
              value,
              size,
              entryType,
              bind,
              visibility,
              ndx,
              if (name.trim.isEmpty) None else Some(name.trim)
            )
          )
        case _ => None // Skip lines that don't match
      }
    entries.toSeq
  }
}

var LabelAddrMap = Map[String, Long]()

def tryRun(code: String): Unit = {
  // try to compile the code at /tmp/GATry.c
  val tmpFile = TmpFiles.try_tmp_file.toString
  val tmpAsmFile = TmpFiles.try_asm_file.toString
  val tmpPatchedAsmFile = TmpFiles.try_patched_asm_file.toString
  val tmpOut = TmpFiles.try_out.toString

  val tmpLinker = TmpFiles.try_linker.toString

  // write the code to the file
  val writer = new java.io.PrintWriter(new java.io.File(tmpFile))
  writer.write(code)
  writer.close()

  // write linker to file
  val linker = LinkerPrefix() + LinkerTryRun() + LinkerPostfix()
  val linkerWriter = new java.io.PrintWriter(new java.io.File(tmpLinker))
  linkerWriter.write(linker)
  linkerWriter.close()

  //println(code)

  // compile the code
  val compile = s"${PATH}${MarchParameters.CrossPrefix}clang ${gcc_param} ${MarchParameters.compileTarget} -o $tmpAsmFile $tmpFile"

  println("C Compile Command: " + compile)

  val compileAsm = s"${PATH}${MarchParameters.CrossPrefix}clang ${MarchParameters.compileTarget} -T ${tmpLinker} -o $tmpOut $tmpPatchedAsmFile"

  println("ASM Compile Command: " + compileAsm)
  // read label info from readelf
  val readelf = s"${MarchParameters.CrossPrefix}readelf -s $tmpOut"

  // run the commands
  val compileResult = sys.process.Process(compile).!

  AsmInserter.insertIntoAsmFile(tmpAsmFile, tmpPatchedAsmFile, branchMapTryRun)

  // compile asm into elf
  val compileAsmResult = sys.process.Process(compileAsm).!

  val readelfResult = sys.process.Process(readelf).!!

  // parse the readelf output
  val symbols = SymbolTableParser.parse(readelfResult)

  // print the symbols
  symbols.foreach(println)

  InterestingLabels.merge() // merge data from control and attacker world, if any

  // for interesting labels, set map between label and offset
  for (label <- InterestingLabels.Labels) {
    val labelEntry = symbols.find(_.name.contains(label))
    if (labelEntry.isDefined) {
      val offset = Integer.parseInt(labelEntry.get.value, 16)
      LabelAddrMap += (label -> offset)
      println(s"Label $label addr ${offset.toHexString}")
    } else {
      //println(s"Label $label not found in the symbol table")
      throw new Exception(s"Label $label not found in the symbol table")
    }
  }
  AdvancedRelocation.relocate = true

  // clean linker script cache
  SMTCode.smtCode = ""
  FuncDeclCode.funcDeclCode = ""
  RemoteCode.remoteCode = ""

}