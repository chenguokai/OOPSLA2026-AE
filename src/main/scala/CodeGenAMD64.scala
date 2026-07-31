package HT.AMD64

import HT.ASTNodes.{ConditionNode, Crc32ComputeNode, LoadNode, StoreNode, ValueNode, WhenNode}
import HT.{AdvancedRelocation, SMTCode, codeGenInternal}
import HT.MarchParameters
import HT.MarchParameters.{CallerReservedRegs, ParamRegs}


def AMD64SpecificCode: String = {
  s"""
     |#include <x86intrin.h>
     |#include <stddef.h>
     |// Flush multiple cache lines starting from ptr
     |static inline void flush_cache_lines(void *ptr, size_t num_lines) {
     |    // Standard x86 cache line is 64 bytes
     |    const size_t CACHE_LINE_SIZE = ${MarchParameters.L1DLine};
     |    unsigned char *p = (unsigned char *)ptr;
     |
     |    for (size_t i = 0; i < num_lines; i++) {
     |        _mm_clflush(p);
     |        p += CACHE_LINE_SIZE;
     |    }
     |    // Single fence at the end is sufficient
     |    _mm_mfence();
     |}
  """.stripMargin
}

def AMD64AsmConditionGen(node: ConditionNode, target: String, currentLabel: String, targetAddr: Option[Long]): String = {
  // generate code for the condition, if not satisfied, jump to target
  val entry =
    s"""
       |__asm__ __volatile__ (
       |   ".global ${currentLabel}Entry\\n\\t"
       |   "${currentLabel}Entry:\\n\\t"
       |);\n
        """.stripMargin
  node.toperator.toperator match {
    case "self" => {
      // generate code for the condition
      val my = codeGenInternal(node.body(0))

      // inline assembly of C
      entry + s"""
         |__asm__ __volatile__ (
         |    "cmp $$0, %0\\n\\t"  // Compare with 0
         |    "${if (targetAddr.isDefined && AdvancedRelocation.relocate) {
        SMTCode.smtCode += s". = 0x${targetAddr.get.toHexString};\n"
        SMTCode.smtCode += s".customtext.${currentLabel} : { *(.customtext.${currentLabel})}\n"
        s".section .customtext.${currentLabel}\\n\\t"}
      else "\\n\\t"
      }"
         |    ".global $currentLabel\\n\\t"
         |    "$currentLabel:\\n\\t"
         |    "je $target"          // Jump if equal
         |    :                        // output operand
         |    : "r" ($my)          // input operand
         |    : "cc"              // clobbered register
         |);
         |""".stripMargin
    }
    case "<" => {
      val left = codeGenInternal(node.body.head)
      val right = codeGenInternal(node.body(1))

      // inline assembly of C
      entry + s"""
         |__asm__ __volatile__ (
         |    "cmp %0, %1\\n\\t"  // Compare left with right, <
         |    "${if (targetAddr.isDefined &&  AdvancedRelocation.relocate) {
        SMTCode.smtCode += s". = 0x${targetAddr.get.toHexString};\n"
        SMTCode.smtCode += s".customtext.${currentLabel} : { *(.customtext.${currentLabel})}\n"
        s".section .customtext.${currentLabel}\\n\\t"}
      else "\\n\\t"
      }"
         |    ".global $currentLabel\\n\\t"
         |    "$currentLabel:\\n\\t"
         |    "jge $target"          // Jump if less
         |    :                        // output operand
         |    : "r" ($left), "r" ($right)          // input operand
         |    : "cc"              // clobbered register
         |);
         |""".stripMargin
    }
    case ">" => {
      val left = codeGenInternal(node.body.head)
      val right = codeGenInternal(node.body(1))

      // inline assembly of C
      entry + s"""
         |__asm__ __volatile__ (
         |    "cmp %0, %1\\n\\t"  // Compare left with right, >
         |"${if (targetAddr.isDefined &&  AdvancedRelocation.relocate) {
        SMTCode.smtCode += s". = 0x${targetAddr.get.toHexString};\n"
        SMTCode.smtCode += s".customtext.${currentLabel} : { *(.customtext.${currentLabel})}\n"
        s".section .customtext.${currentLabel}\\n\\t"}
      else "\\n\\t"
      }"
         |    ".global $currentLabel\\n\\t"
         |    "$currentLabel:\\n\\t"
         |    "jle $target"          // Jump if greater
         |    :                        // output operand
         |    : "r" ($left), "r" ($right)          // input operand
         |    : "cc"              // clobbered register
         |);
         |""".stripMargin
    }
    case "==" => {
      val left = codeGenInternal(node.body.head)
      val right = codeGenInternal(node.body(1))

      // inline assembly of C
      entry + s"""
         |__asm__ __volatile__ (
         |    "cmp %0, %1\\n\\t"  // Compare left with right, ==
         |"${if (targetAddr.isDefined &&  AdvancedRelocation.relocate) {
        SMTCode.smtCode += s". = 0x${targetAddr.get.toHexString};\n"
        SMTCode.smtCode += s".customtext.${currentLabel} : { *(.customtext.${currentLabel})}\n"
        s".section .customtext.${currentLabel}\\n\\t"}
      else "\\n\\t"
      }"
         |    ".global $currentLabel\\n\\t"
         |    "$currentLabel:\\n\\t"
         |    "jne $target"          // Jump if not equal
         |    :                        // output operand
         |    : "r" ($left), "r" ($right)          // input operand
         |    : "cc"              // clobbered register
         |);
         |""".stripMargin
    }
    case _ => {
      throw new Exception("Unsupported operator in AsmConditionGen: " + node.toperator.toperator)
    }
  }
}

def AMD64WhenTailfix(node: WhenNode) = s"""
                          |__asm__ __volatile__ (
                          |    "jmp target${node.uniname}\\n\\t" // jump to end of when
                          |);
              """.stripMargin

def AMD64LoadParam(i: Int) = {
  s"""    "mov %[${('a' + i).toChar}], %%${ParamRegs(i)}\\n\\t"   """
}

def AMD64ReserveRegStr =
  s"""  : "rsp", "rax", ${ParamRegs.map { a => s""""${a}""""}.mkString(", ")}, ${CallerReservedRegs.map { a => s""""${a}"""}.mkString(", ")}, "memory" """


def AMD64PushCode(i: Int) =
  s"""    "push %[push${('a' + i).toChar}]\\n\\t" """

def AMD64LoadCallAddr = s"""    "mov %[func], %%rax\\n\\t"  // Move function addr to rax"""

def AMD64RealCall = s"""    "call *%%rax\\n\\t"  // Call function"""

def AMD64SpRecover(pushCount: Int) = s"add $$${pushCount * 8}, %%rsp\\n\\t"

def AMD64MFence = "asm volatile(\"mfence\");"

def AMD64LoadNoLabel(node: LoadNode) = s"""
                          |__asm__ __volatile__ (
                          |    ".global ${node.uniname}\\n\\t"
                          |    "${node.uniname}:\\n\\t"
                          |    "mov (%1), %0"      // Move rax to tmp
                          |    : "=r" (${node.dst.name})          // output operand
                          |    : "r" (&${node.src.name})          // input operand
                          |);
                          |""".stripMargin

def AMD64PtrLoad(ptr: ValueNode, dst: ValueNode) =
  s"""
     |__asm__ __volatile__ (
     |    "mov (%1), %0"      // Move rax to tmp
     |    : "=r" (${dst.name})          // output operand
     |    : "r" (${ptr.name})          // input operand
     |);
     |""".stripMargin

def AMD64LoadLabel(node: LoadNode, labelName: String) = s"""
                                        |__asm__ __volatile__ (
                                        |    ".section .customtext.${labelName},\\"ax\\",@progbits\\n\\t"
                                        |    ".global ${labelName}\\n\\t"
                                        |    "${labelName}:\\n\\t"
                                        |    "mov (%1), %0"      // Move rax to tmp
                                        |    : "=r" (${node.dst.name})          // output operand
                                        |    : "r" (&${node.src.name})          // input operand
                                        |);
                                        |""".stripMargin

def AMD64StoreNoLabel(node: StoreNode) = s"""
                                            |__asm__ __volatile__ (
                                            |    ".global ${node.uniname}\\n\\t"
                                            |    "${node.uniname}:\\n\\t"
                                            |    "mov %0, (%1)"      // Move tmp to rax
                                            |    :                        // output operand
                                            |    : "r" (${node.src.name}), "r" (${if (node.noderef) "" else "&"}${node.dst.name})          // input operand
                                            |);
                                            |""".stripMargin

def AMD64StoreLabel(node: StoreNode, labelName: String) = s"""
                                            |__asm__ __volatile__ (
                                            |    ".section .customtext.${labelName},\\"ax\\",@progbits\\n\\t"
                                            |    ".global ${labelName}\\n\\t"
                                            |    "${labelName}:\\n\\t"
                                            |    "mov %0, (%1)"      // Move tmp to rax
                                            |    :                        // output operand
                                            |    : "r" (${node.src.name}), "r" (${if (node.noderef) "" else "&"}${node.dst.name})          // input operand
                                            |);
                                            |""".stripMargin

def AMD64RDTSC = s"""
                    |static __inline__ int64_t rdtsc(void)
                    |{
                    |  unsigned a, d;
                    |  asm volatile("mfence");
                    |  asm volatile("rdtsc" : "=a" (a), "=d" (d));
                    |  asm volatile("mfence");
                    |  return ((unsigned long)a) | (((unsigned long)d) << 32);
                    |}
                    """.stripMargin

def AMD64FENCEI = s"""
|extern void flush_icache();
|__asm__ (
|".macro single_line\\n"
|"    jmp 1f\\n"
|"    .rept 62\\n"
|"    nop\\n"
|"    .endr\\n"
|"1:\\n"
|".endm\\n"
|".global flush_icache\\n"
|".p2align 20\\n"
|"flush_icache:\\n"
|"    .rept 1023 \\n"
|"    single_line\\n"
|"    .endr\\n"
|"    ret\\n"
|);
""".stripMargin

def AMD64FlushBpHistory = {
  if (MarchParameters.MarchName == "AMDZen4") {
    s"""
       |__asm__ volatile (
       |        "mov $$${MarchParameters.BPHistorySize}, %%rax\\n\\t"     // Set rax to 10 (change this constant as needed)
       |        "1:\\n\\t"
       |        "dec %%rax\\n\\t"          // Decrement rax
       |        "cmp $$0, %%rax\\n\\t"      // Compare rax with 0
       |        "jne 1b\\n\\t"             // If not zero, jump back to label 1
       |        :                        // Output
       |        :                        // No input
       |        : "%rax"                 // Clobbered register
       |    );
    """.stripMargin
  } else {
    s"""
       |__asm__ __volatile__ (
       |     ".rept ${MarchParameters.BPHistorySize}\\n\\t"
       |     "jmp 1f\\n\\t"
       |     "1:\\n\\t"
       |     ".endr\\n\\t"
       |);
      """.stripMargin
  }

}

def AMD64DCacheFlush(node: ValueNode, lines: Int): String = {
  s"""
     |flush_cache_lines(&${node.name}, ${lines});
  """.stripMargin
}

def AMD64DCacheFlushPtr(node: ValueNode): String = {
  s"""
     |flush_cache_lines(${node.name}, 1);
     |""".stripMargin
}

def AMD64SequentialComputingDelay(src: ValueNode, rep: Int) = {
  s"""
     |asm volatile (
     |        "movq %1, %%rax\\n\\t"    // move input value to rax
     |        "movq $$2, %%rcx\\n\\t"    // put divisor (2) in rcx
     |        ".rept ${rep}\\n\\t"
     |        "xorq %%rdx, %%rdx\\n\\t" // clear rdx for division
     |        "divq %%rcx\\n\\t"        // divide rdx:rax by 2
     |        "shlq $$1, %%rax\\n\\t"    // left shift by 1
     |        ".endr\\n\\t"
     |        "movq %%rax, %0\\n\\t"    // move result to output
     |        : "=r" (${src.name})         // output can be any register
     |        : "r" (${src.name})          // input can be any register
     |        : "rax", "rdx", "rcx"   // clobbers
     |    );
  """.stripMargin
}

def AMD64Crc32ComputeGen(node: Crc32ComputeNode) = {
  val ptr = node.src
  val key = node.key
  // Ensure the two lists have the same number of elements.
  require(ptr.size == key.size, "ptr and key must have the same length")
  val count = ptr.size

  // Generate the inline assembly instruction lines.
  // For each round (we have two rounds here) and for each register index,
  // we produce a line like: "crc32 %[k0], %[a0]\n\t"
  val instructions: String =
    (1 to 2).flatMap { _ =>
      (0 until count).map { i =>
        s""""crc32 %[k$i], %[a$i]\\n\\t""""
      }
    }.mkString("\n")

  // Generate the output constraints for the pointer registers.
  // Each element becomes something like: [a0] "+r" (a0)
  val outputConstraints: String =
    ptr.zipWithIndex.map { case (p, i) =>
      s"[a$i] \"+r\" (${p.name})"
    }.mkString(", ")

  // Generate the input constraints for the key registers.
  // Each element becomes something like: [k0] "r" (k0)
  val inputConstraints: String =
    key.zipWithIndex.map { case (k, i) =>
      s"[k$i] \"r\" (${k.name})"
    }.mkString(", ")

  // Assemble the final inline assembly string.
  s"""__asm__ volatile(
     |  $instructions
     |  : $outputConstraints
     |  : $inputConstraints
     |);""".stripMargin
}

val branchJumpKeywordsAMD64 = Set(
  // Unconditional jumps
  "jmp",           // Jump directly

  // Conditional jumps (signed comparisons)
  "je", "jz",      // Jump if equal / zero
  "jne", "jnz",    // Jump if not equal / not zero
  "jg",            // Jump if greater (signed)
  "jge",           // Jump if greater or equal (signed)
  "jl",            // Jump if less (signed)
  "jle",           // Jump if less or equal (signed)

  // Conditional jumps (unsigned comparisons)
  "ja",            // Jump if above (unsigned greater)
  "jae",           // Jump if above or equal (unsigned greater or equal)
  "jb",            // Jump if below (unsigned less)
  "jbe",           // Jump if below or equal (unsigned less or equal)

  // Parity and carry flag jumps
  "jp", "jpe",     // Jump if parity / parity even
  "jnp", "jpo",    // Jump if no parity / parity odd
  "jc",            // Jump if carry
  "jnc",           // Jump if no carry

  // Sign flag jumps
  "js",            // Jump if sign (negative)
  "jns",           // Jump if no sign (non-negative)

  // Overflow flag jumps
  "jo",            // Jump if overflow
  "jno",           // Jump if no overflow

  // Procedure calls and returns
  "call",          // Call procedure
  "ret",           // Return from procedure
  "syscall",       // System call
  "int"            // Interrupt
)

