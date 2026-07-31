package HT.RISCV64

import HT.ASTNodes.{ArrayNode, AsmOperand, ConditionNode, LoadNode, ObjectValueNode, StoreNode, ValueNode, WhenNode}
import HT.{AdvancedRelocation, SMTCode, codeGenInternal}
import HT.MarchParameters
import HT.MarchParameters.{CallerReservedRegs, ParamRegs}
import HT.StdLib.InlineAsm

def RISCV64SpecificCode: String = {
  s"""
     |#include <stddef.h>
     |
     |// Flush multiple cache lines starting from ptr
     |static inline void flush_cache_lines(void *ptr, size_t num_lines) {
     |    // RISC-V cache line size is typically 64 bytes
     |    const size_t CACHE_LINE_SIZE = ${MarchParameters.L1DLine};
     |    unsigned char *p = (unsigned char *)ptr;
     |
     |    for (size_t i = 0; i < num_lines; i++) {
     |        // CBO.FLUSH instruction - clean and invalidate
     |        asm volatile("cbo.flush (%0)" :: "r"(p));
     |        p += CACHE_LINE_SIZE;
     |    }
     |    // Ensure all flushes are complete
     |    asm volatile("fence" ::: "memory");
     |}
  """.stripMargin
}

def RISCV64AsmConditionGen(node: ConditionNode, target: String, currentLabel: String, targetAddr: Option[Long]): String = {
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
         |    "${if (targetAddr.isDefined && AdvancedRelocation.relocate) {
        SMTCode.smtCode += s". = 0x${targetAddr.get.toHexString};\n"
        SMTCode.smtCode += s".customtext.${currentLabel} : { *(.customtext.${currentLabel})}\n"
        s""".section .customtext.${currentLabel},\\"ax\\",@progbits\\n\\t"""}
      else "\\n\\t"
      }"
         |    ".global $currentLabel\\n\\t"
         |    "$currentLabel:\\n\\t"
         |    "beq x0, %0, $target"          // Jump if equal
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
         |    "${if (targetAddr.isDefined &&  AdvancedRelocation.relocate) {
        SMTCode.smtCode += s". = 0x${targetAddr.get.toHexString};\n"
        SMTCode.smtCode += s".customtext.${currentLabel} : { *(.customtext.${currentLabel})}\n"
        s".section .customtext.${currentLabel}\\n\\t"}
      else "\\n\\t"
      }"
         |    ".global $currentLabel\\n\\t"
         |    "$currentLabel:\\n\\t"
         |    "bge %0, %1, $target"          // Jump if less
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
         |"${if (targetAddr.isDefined &&  AdvancedRelocation.relocate) {
        SMTCode.smtCode += s". = 0x${targetAddr.get.toHexString};\n"
        SMTCode.smtCode += s".customtext.${currentLabel} : { *(.customtext.${currentLabel})}\n"
        s".section .customtext.${currentLabel}\\n\\t"}
      else "\\n\\t"
      }"
         |    ".global $currentLabel\\n\\t"
         |    "$currentLabel:\\n\\t"
         |    "ble %0, %1, $target"          // Jump if greater
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
         |"${if (targetAddr.isDefined &&  AdvancedRelocation.relocate) {
        SMTCode.smtCode += s". = 0x${targetAddr.get.toHexString};\n"
        SMTCode.smtCode += s".customtext.${currentLabel} : { *(.customtext.${currentLabel})}\n"
        s".section .customtext.${currentLabel}\\n\\t"}
      else "\\n\\t"
      }"
         |    ".global $currentLabel\\n\\t"
         |    "$currentLabel:\\n\\t"
         |    "bne %0, %1, $target"          // Jump if not equal
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

def RISCV64WhenTailfix(node: WhenNode) = s"""
                            |__asm__ __volatile__ (
                            |    "jmp target${node.uniname}\\n\\t" // jump to end of when
                            |);
              """.stripMargin

def RISCV64LoadParam(i: Int) = {
  s"""    "mv ${ParamRegs(i)}, %[${('a' + i).toChar}]\\n\\t"   """
}

def RISCV64ReserveRegStr = s""" : "sp", "ra", ${ParamRegs.map {a => s""""${a}""""}.mkString(", ")}, ${CallerReservedRegs.map { a => s""""${a}""""}.mkString(", ")}, "memory" """

def RISCV64PushCode(i: Int) =
  s"""    "sd %[push${('a' + i).toChar}], (sp)\\n\\t"\n\t"addi sp, sp, 8" """

def RISCV64LoadCallAddr(name: String) = s"""    "auipc ra, %%pcrel_hi(${name})\\n\\t" """

def RISCV64RealCall(name: String) = s"""   "jalr %%pcrel_lo(${name})(ra)" """

def RISCV64SpRecover(pushCount: Int) = s"addi sp, sp, $$${pushCount * 8}\\n\\t"

def RISCV64MFence = "asm volatile(\"fence\");"

def RISCV64LoadNoLabel(node: LoadNode) = s"""
                                          |__asm__ __volatile__ (
                                          |    ".global ${node.uniname}\\n\\t"
                                          |    "${node.uniname}:\\n\\t"
                                          |    "ld %0, (%1)\\n\\t"      // Move rax to tmp
                                          |    : "=r" (${node.dst.name})          // output operand
                                          |    : "r" (&${node.src.name})          // input operand
                                          |);
                                          |""".stripMargin

def RISCV64PtrLoad(ptr: ValueNode, dst: ValueNode) =
  s"""
     |__asm__ __volatile__ (
     |    "ld %0, (%1)\\n\\t"      // Move rax to tmp
     |    : "=r" (${dst.name})          // output operand
     |    : "r" (${ptr.name})          // input operand
     |);
     |""".stripMargin

def RISCV64LoadLabel(node: LoadNode, labelName: String) = s"""
                                                           |__asm__ __volatile__ (
                                                           |    ".section .customtext.${labelName},\\"ax\\",@progbits\\n\\t"
                                                           |    ".global ${labelName}\\n\\t"
                                                           |    "${labelName}:\\n\\t"
                                                           |    "ld %0, (%1)"      // Move rax to tmp
                                                           |    : "=r" (${node.dst.name})          // output operand
                                                           |    : "r" (&${node.src.name})          // input operand
                                                           |);
                                                           |""".stripMargin

def RISCV64StoreNoLabel(node: StoreNode) = s"""
                                              |__asm__ __volatile__ (
                                              |    ".global ${node.uniname}\\n\\t"
                                              |    "${node.uniname}:\\n\\t"
                                              |    "sd %0, (%1)"      // Move tmp to rax
                                              |    :                        // output operand
                                              |    : "r" (${node.src.name}), "r" (${if (node.noderef) "" else "&"}${node.dst.name})          // input operand
                                              |);
                                              |""".stripMargin

def RISCV64StoreLabel(node: StoreNode, labelName: String) = s"""
                                                             |__asm__ __volatile__ (
                                                             |    ".section .customtext.${labelName},\\"ax\\",@progbits\\n\\t"
                                                             |    ".global ${labelName}\\n\\t"
                                                             |    "${labelName}:\\n\\t"
                                                             |    "sd %0, (%1)"      // Move tmp to rax
                                                             |    :                        // output operand
                                                             |    : "r" (${node.src.name}), "r" (${if (node.noderef) "" else "&"}${node.dst.name})          // input operand
                                                             |);
                                                             |""".stripMargin

def RISCV64RDTSC = s"""
                      |static inline uint64_t rdtsc() {
                      |    uint64_t tmp;
                      |    __asm__ __volatile__ (
                      |        "rdcycle %0"      // Move rax to tmp
                      |        : "=r" (tmp)          // output operand
                      |        :                        // input operand
                      |        : "t0"              // clobbered register
                      |    );
                      |    return tmp;
                      |}
                      |#define DISABLE_TIME_INTR 0x100
                      |#define NOTIFY_PROFILER 0x101
                      |#define GOOD_TRAP 0x0
                      |
                      |void nemu_signal(int a){
                      |    asm volatile ("mv a0, %0\\n\\t"
                      |                  ".insn r 0x6B, 0, 0, x0, x0, x0\\n\\t"
                      |                  :
                      |                  : "r"(a)
                      |                  : "a0");
                      |}
                      |""".stripMargin

def RISCV64FENCEI = s"""
|void flush_icache() {
|  __asm__ __volatile__ ("fence.i");
|}
""".stripMargin

def RISCV64FlushBpHistory = {
  s"""
  |__asm__ __volatile__ (
  |    ".rept ${MarchParameters.BPHistorySize}\\n\\t"
  |    "beq x0, x0, 1f\\n\\t"
  |    "1:\\n\\t"
  |    ".endr\\n\\t"
  |);
  """.stripMargin
}

def RISCV64DCacheFlush(node: ValueNode, lines: Int): String = {
  s"""
     |flush_cache_lines(&${node.name}, ${lines});
  """.stripMargin
}

def RISCV64DCacheFlushPtr(node: ValueNode): String = {
  s"""
     |flush_cache_lines(${node.name}, 1);
     |""".stripMargin
}

def RISCV64SequentialComputingDelay(src: ValueNode, rep: Int): String = {
  s"""
     |asm volatile (
     |        "li t0, 2\\n\\t"          // load immediate 2 into t0
     |        ".rept ${rep}\\n\\t"
     |        "div %1, %1, t0\\n\\t"    // divide by 2
     |        "slli %1, %1, 1\\n\\t"    // left shift by 1
     |        ".endr\\n\\t"
     |        "mv %0, %1\\n\\t"         // move result to output
     |        : "=r" (${src.name})         // output
     |        : "r" (${src.name})          // input (same as output register)
     |        : "t0"                  // clobbers
     |    );
  """.stripMargin
}

val branchJumpKeywordsRISCV64 = Set(
  // Conditional branches (register comparison)
  "beq",   // Branch if equal
  "bne",   // Branch if not equal
  "blt",   // Branch if less than (signed)
  "bltu",  // Branch if less than (unsigned)
  "bge",   // Branch if greater than or equal (signed)
  "bgeu",  // Branch if greater than or equal (unsigned)

  "call",  // Call a procedure
  // Unconditional jumps
  "j",     // Jump
  "jr",    // Jump register

  // Function calls and returns
  "jal",   // Jump and link (procedure call)
  "jalr",  // Jump and link register
  "ret",   // Return from procedure

  // System calls and environment calls
  "ecall", // Environment call (system call)
  "ebreak" // Environment break
)

def rvRegToInt(regName: String): Int = {
  regName.toLowerCase match {
    // Zero register
    case "zero" | "x0" => 0

    // Return address register
    case "ra" | "x1" => 1

    // Stack pointer and global pointer
    case "sp" | "x2" => 2
    case "gp" | "x3" => 3

    // Thread pointer and temporaries
    case "tp" | "x4" => 4
    case "t0" | "x5" => 5
    case "t1" | "x6" => 6
    case "t2" | "x7" => 7

    // Stored/frame pointer and saved registers
    case "fp" | "s0" | "x8" => 8
    case "s1" | "x9" => 9

    // Function arguments / return values
    case "a0" | "x10" => 10
    case "a1" | "x11" => 11
    case "a2" | "x12" => 12
    case "a3" | "x13" => 13
    case "a4" | "x14" => 14
    case "a5" | "x15" => 15
    case "a6" | "x16" => 16
    case "a7" | "x17" => 17

    // Saved registers
    case "s2" | "x18" => 18
    case "s3" | "x19" => 19
    case "s4" | "x20" => 20
    case "s5" | "x21" => 21
    case "s6" | "x22" => 22
    case "s7" | "x23" => 23
    case "s8" | "x24" => 24
    case "s9" | "x25" => 25
    case "s10" | "x26" => 26
    case "s11" | "x27" => 27

    // Temporaries
    case "t3" | "x28" => 28
    case "t4" | "x29" => 29
    case "t5" | "x30" => 30
    case "t6" | "x31" => 31

    // Direct x-register format
    case r if r.startsWith("x") =>
      try {
        val num = r.substring(1).toInt
        if (num >= 0 && num <= 31) num else throw new IllegalArgumentException(s"Invalid register number: $num")
      } catch {
        case _: NumberFormatException => throw new IllegalArgumentException(s"Invalid register name: $regName")
      }

    case _ => throw new IllegalArgumentException(s"Unknown register name: $regName")
  }
}

def ZicondSelAdd(counter: ValueNode, cond: ValueNode, A: ValueNode, B: ValueNode, handle: ObjectValueNode) = {
  InlineAsm(
    body = List(
      "andi t0, %1, 1",
      "czero.eqz t1, %2, t0",
      "czero.nez t2, %3, t0",
      "add t1, t1, t2",
      "add %0, %0, t1"),
    outputs = List(AsmOperand("+r", counter)),
    inputs = List(AsmOperand("r", cond), AsmOperand("r", A), AsmOperand("r", B)),
    clobbers = List("t0", "t1", "t2"),
    handle = handle
  )
}