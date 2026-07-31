import scala.io.Source
import scala.util.matching.Regex
import java.io.File

// Main data structures to store the parsed information
case class Instruction(
                        address: String,
                        instructionType: InstructionType,
                        raw: String
                      )

case class Symbol(
                   name: String,
                   address: String,
                   instructions: List[Instruction]
                 )

// Enum for different instruction types
sealed trait InstructionType
case object ConditionalBranch extends InstructionType
case object StoreType extends InstructionType
case object LoadType extends InstructionType
case object UnconditionalJump extends InstructionType
case object IndirectJump extends InstructionType
case object Other extends InstructionType

// Main class to handle the parsing
class DisassemblyParser {
  // Regex patterns for parsing
  private val symbolPattern = """([0-9a-f]+)\s+<(.+)>:""".r
  private val instructionPattern = """^\s*([0-9a-f]+):\s+(.+)$""".r

  private trait ISAPattern {
    val conditionalBranch: Regex
    val store: Regex
    val load: Regex
    val unconditionalJump: Regex
    val indirectJump: Regex
  }

  // Architecture specific patterns
  private object AMD64Patterns extends ISAPattern {
    val conditionalBranch = """^(?:jo|jno|jb|jae|je|jne|jc|jnc|jz|jnz|jbe|ja|js|jns|jpe|jpo|jl|jge|jle|jg)[\s\S]*$""".r // jle, je, jne, jg, etc.
    val store = """^mov[\s\S]*,[\s\S]*\([\s\S]*\)""".r        // mov to memory
    val load = """^mov[\s\S]*\([\s\S]*\),[\s\S]*""".r
    val unconditionalJump = """^(?:call|jmp)[^*]*$""".r
    val indirectJump = """^(?:call|jmp)[\s\S]*\\*[\s\S]*$""".r
  }

  private object RISCVPatterns extends ISAPattern {
    val conditionalBranch = """^(?:beq|bne|blt|bge|bltu|bgeu|bgez|bltz|bgtz|blez|beqz|bnez)$""".r  // beq, bne, blt, etc.
    val store = """^s[bdwh]""".r                // sb, sd, sw, sh
    val load = """^l[bwdhl][bu]?""".r  // lb, lbu, ld, ldu, lw, lwu, lh, lhu
    val unconditionalJump = """^(?:jal|j)$""".r
    val indirectJump = """^(?:jalr|jr)$""".r // jalr and jr
  }

  // Determine ISA based on file content
  def determineISA(content: String): String = {
    if (content.contains("elf64") && content.contains("riscv")) "RISC-V64"
    else if (content.contains("elf64") && content.contains("x86-64")) "AMD64"
    else throw new Exception("Unknown ISA")
  }

  // Determine instruction type based on ISA and instruction text
  private def determineInstructionType(instruction: String, isa: String): InstructionType = {
    val patterns = isa match {
      case "AMD64" => AMD64Patterns
      case "RISC-V64" => RISCVPatterns
      case _ => throw new Exception("Unsupported ISA")
    }

    instruction match {
      case patterns.conditionalBranch() => ConditionalBranch
      case patterns.store() => StoreType
      case patterns.load() => LoadType
      case patterns.unconditionalJump() => UnconditionalJump
      case patterns.indirectJump() => IndirectJump
      case _ => Other
    }
  }

  // Parse the entire file
  def parse(filePath: String): (String, List[Symbol]) = {
    val content = Source.fromFile(filePath).mkString
    val isa = determineISA(content)

    var currentSymbol: Option[Symbol] = None
    var symbols = List[Symbol]()
    var currentInstructions = List[Instruction]()

    for (line <- content.split("\n")) {
      line match {
        case symbolPattern(addr, name) =>
          // Save previous symbol if exists
          currentSymbol.foreach(sym =>
            symbols = sym.copy(instructions = currentInstructions.reverse) :: symbols
          )
          currentSymbol = Some(Symbol(name, addr, List()))
          currentInstructions = List()

        case instructionPattern(addr, inst) if currentSymbol.isDefined =>
          if (!inst.trim.startsWith("...")) {
            val sinst = if (isa == "RISC-V64") inst.trim().split("\t", 2).last.split("\t", 2).head else inst.trim().split("\t", 2).last
            val instructionType = determineInstructionType(sinst, isa)
            currentInstructions = Instruction(addr, instructionType, inst.trim) :: currentInstructions
          }

        case _ => // Skip other lines
      }
    }

    // Don't forget to add the last symbol
    currentSymbol.foreach(sym =>
      symbols = sym.copy(instructions = currentInstructions.reverse) :: symbols
    )

    (isa, symbols.reverse)
  }
}

// Main object to demonstrate usage
import java.io.PrintWriter

// Main object to demonstrate usage
object DisassemblyAnalyzer {
  def analyzeFile(filePath: String): Unit = {
    val outputWriter = new PrintWriter("/tmp/output.txt")
    val parser = new DisassemblyParser()
    try {
      val (isa, symbols) = parser.parse(filePath)
      outputWriter.println(s"Detected ISA: $isa")
      outputWriter.println("Symbols found:")
      symbols.foreach { symbol =>
        outputWriter.println(s"\nSymbol: ${symbol.name} at address ${symbol.address}")
        if (symbol.instructions.isEmpty) {
          outputWriter.println("  No valid instructions found")
        } else {
          symbol.instructions.foreach { inst =>
            outputWriter.println(f"  ${inst.address}%8s: ${inst.instructionType}%-15s ${inst.raw}")
          }
        }
      }
    } catch {
      case e: Exception =>
        outputWriter.println(s"Error processing file: ${e.getMessage}")
    } finally {
      outputWriter.close()
    }
  }

  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      System.err.println("Usage: scala DisassemblyAnalyzer <path-to-objdump-file>")
      System.exit(1)
    } else {
      analyzeFile(args(0))
    }
  }
}