package HT.AsmPatcher

// We patch generated assembly code annoated with labels and add our section code

import HT.AMD64.branchJumpKeywordsAMD64
import HT.RISCV64.branchJumpKeywordsRISCV64
import HT.{MarchParameters, applyXiangShan2ndGenParam}

import scala.io.Source
import scala.collection.mutable
import java.io.{File, PrintWriter}

object AsmInserter {
  // Custom exception for insertion errors
  class InsertionException(message: String) extends Exception(message)

  // Identify if a line is a branch or jump instruction
  private def isBranchOrJump(line: String): Boolean = {
    val branchJumpKeywords = MarchParameters.ISA match {
      case "x86_64" => branchJumpKeywordsAMD64
      case "riscv64" => branchJumpKeywordsRISCV64
      case _ => throw new IllegalArgumentException(s"Unsupported architecture: ${MarchParameters.ISA}")
    }
    branchJumpKeywords.exists(keyword => line.trim.toLowerCase.startsWith(keyword))
  }

  // Perform insertion into ASM file
  def insertIntoAsmFile(
                         inputFile: String,
                         outputFile: String,
                         insertions: mutable.Map[String, String]
                       ): Unit = {
    // Validate inputs
    require(inputFile != null && outputFile != null && insertions != null,
      "Input parameters cannot be null")

    val sourceLines = Source.fromFile(inputFile).getLines().toList
    val outputLines = new mutable.ListBuffer[String]()
    val insertionDetails = new mutable.ListBuffer[(String, String)]()

    // Process lines with potential insertions
    var i = 0
    val len = sourceLines.length
    val sourceLinesVec = sourceLines.toVector
    while (i < len) {
      val line = sourceLinesVec(i)

      // Check for insertion points
      val matchingInsertions = insertions.filter { case (label, _) =>
        line.trim.startsWith(s"$label:")
      }

      if (matchingInsertions.nonEmpty) {
        // Add the label line first
        outputLines += line

        // Process each matching insertion
        matchingInsertions.foreach { case (insertionLabel, insertionContent) =>
          // Find lines after the insertion label
          val linesAfterLabel = sourceLinesVec.slice(i + 1, len)
          
          
          val linesAfter = linesAfterLabel.takeWhile(_.trim.startsWith(".global"))
          // Check for .global label before first branch/jump
          val branchExistenceCheck = !(linesAfterLabel.map{
            a => isBranchOrJump(a)
          }.reduce(_ || _))
          
          if (branchExistenceCheck) {
            throw new InsertionException(
              s"Cannot insert: .global label found before first branch/jump for $insertionLabel"
            )
          }

          // Find first branch/jump insertion point
          val insertIndex = {
            var idx = linesAfterLabel.indexWhere(isBranchOrJump)
            if (!insertionLabel.endsWith("_jump")) {
              while (linesAfterLabel(idx).contains("jmp\t.LBB") || linesAfterLabel(idx).contains("j\t.LBB")) {
                // skip some cases where compiler wants to jump to some following block
                // examples include if: they may have some naive jumps with O0
                idx = linesAfterLabel.indexWhere(isBranchOrJump, idx + 1)
              }
            }
            idx
          }


          if (insertIndex == -1 || insertIndex == linesAfterLabel.length) {
            throw new InsertionException(
              s"No branch/jump instruction found after label $insertionLabel"
            )
          }

          // add to output until insertIndex
          outputLines ++= linesAfterLabel.take(insertIndex)

          i += insertIndex

          // Store insertion details
          insertionDetails += ((insertionLabel, insertionContent))

          // Insert the content just before the branch/jump
          outputLines += insertionContent
        }
      } else {
        // For non-insertion lines, simply add to output
        outputLines += line
      }

      i += 1
    }

    // Write modified content to output file
    val writer = new PrintWriter(new File(outputFile))
    try {
      outputLines.foreach(writer.println)
    } finally {
      writer.close()
    }

    // Print insertion details
    insertionDetails.foreach { case (label, content) =>
      println(s"Inserted into $label: $content")
    }
  }

  // Main method for demonstration
  def main(args: Array[String]): Unit = {
    applyXiangShan2ndGenParam()
    val insertions = mutable.Map(
      "LABEL1" -> "\t# Custom insertion before first branch",
      "LABEL2" -> "\t# Another insertion content"
    )

    try {
      insertIntoAsmFile("/home/xim-intel14/SecurityDSL/patch.s", "/home/xim-intel14/SecurityDSL/output.s", insertions)
      println("ASM file successfully modified.")
    } catch {
      case e: InsertionException =>
        println(s"Insertion Error: ${e.getMessage}")
      case e: Exception =>
        println(s"Unexpected error: ${e.getMessage}")
    }
  }
}