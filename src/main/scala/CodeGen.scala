package HT

import HT.ASTNodes.*
import HT.AttackerZones.Sequential
import HT.Linux.{noinlineC, sectionStringC, sectionTextStringC, sectionTextStringCNoAX, startProcess, startThread}
import HT.AMD64.*
import HT.RISCV64.*
import HT.*
import HT.CodeGen.{CodeType2C, CodeType2Z3, FuncPlacement, Offset}
import HT.MarchParameters.{CallerReservedRegs, MarchName, MaxParamReg, PaddingMaxGap, ParamRegs, SMTCore}
import HT.Types.{ArrayElementNode, AsmBlock, Atomic, Bool, Cacheline, ControlflowInst, ExactPaddingNode, HTArray, Imm, Int16, Int32, Int64, Int8, JmpNode, LabelNode, LoadInst, PaddingNode, SInt, StoreInst, TLBEntry, UInt, UInt16, UInt32, UInt64, UInt8, types}
import HT.Types.types.CacheLine

import scala.collection.mutable
import scala.collection.mutable.Map

var branchMapTryRun = mutable.Map[String, String]()
var branchMap = mutable.Map[String, String]()

object SMTCode {
  private[HT] var smtCode: String = ""

  def getSMTCode(): String = smtCode
}

object RemoteCode {
  private[HT] var remoteCode: String = ""

  def getRemoteCode(): String = remoteCode
}

object FuncDeclCode {
  private[HT] var funcDeclCode: String = ""

  def getFuncDeclCode(): String = funcDeclCode
}

object AdvancedRelocation {
  var relocate: Boolean = false
}

private val FunctionObjectGapLimitBytes: Long = 0x2000

private def functionObjectAddrName(obj: ObjectDeclareNode): String = {
  obj.body match {
    case b: ControlflowInst => b.uniname
    case l: LoadInst => l.uniname
    case s: StoreInst => s.uniname
    case l: LabelNode => l.name
    case a: AsmBlock => a.uniname
    case _ => throw new Exception("Unknown object type within function placement objects " + obj.body)
  }
}

private def checkFunctionObjectGaps(node: FunctionDeclNode): Unit = {
  val objectAddrs = node.placementObjects
    .map(obj => functionObjectAddrName(obj))
    .distinct
    .flatMap(name => UniqueNameAddrMap.get(name).map(addr => (name, addr)))
    .sortBy(_._2)

  objectAddrs.sliding(2).foreach {
    case List((prevName, prevAddr), (nextName, nextAddr)) =>
      val gap = nextAddr - prevAddr
      if (gap > FunctionObjectGapLimitBytes) {
        throw new Exception(
          s"Function ${node.name} has a huge object address gap: " +
            s"$prevName at 0x${prevAddr.toHexString} and $nextName at 0x${nextAddr.toHexString} " +
            s"are $gap bytes apart, exceeding $FunctionObjectGapLimitBytes bytes"
        )
      }
    case _ =>
  }
}

private def labeledOperationPrefix(labelName: String, padding: Option[PaddingNode]): String = {
  if (AdvancedRelocation.relocate && SATParamSet.contains(labelName)) {
    val addr = UniqueNameAddrMap(labelName)
    SMTCode.smtCode += ". = 0x" + addr.toHexString + ";\n"
    SMTCode.smtCode += ".customtext." + labelName + " : { *(.customtext." + labelName + ")}\n"
    if (FuncPlacement.funcFirst) {
      FuncPlacement.funcFirst = false
    } else {
      val expect_off = addr - FuncPlacement.funcBase
      val off = expect_off - Offset(FuncPlacement.funcName, labelName)
      if (off < 0) {
        throw new Exception("Placement rule cannot be satisfied where actual code size exceeded at " + FuncPlacement.funcName + " " + labelName)
      }
      if (padding.isDefined) {
        PaddingManager.mapAppend(padding.get.name, if (off - PaddingMaxGap > 0) (off - PaddingMaxGap) else 0)
      }
      FuncPlacement.funcBase += off
    }
    """
       |asm(
       |    ".section .customtext.""" + labelName + """,\"ax\",@progbits\n\t"
       |    ".global """ + labelName + """\n\t"
       |    """ + labelName + """:\n\t"
       |);
       |""".stripMargin
  } else {
    """
       |asm(".global """ + labelName + """\n\t"
       |    """ + labelName + """:\n\t");
       |""".stripMargin
  }
}

private def labelOperation(inst: Option[ObjectValueNode], padding: Option[PaddingNode], uniname: String, code: String): String = {
  if (inst.isDefined) {
    labeledOperationPrefix(uniname, padding) + code
  } else {
    code
  }
}

def paddingMacro(): String = {
  // iterate over all padding elements, if found in map, define as value in map
  // otherwise define as 0
  PaddingManager.paddingSet.map {
    case name => {
      println(s"$name ${PaddingManager.mapFind(name)}")
      if (AdvancedRelocation.relocate)
        s"#define ${name}COUNT ${PaddingManager.mapFind(name)}"
      else
        s"#define ${name}COUNT 0"
    }
  }.mkString("\n")
}

def AsmReturnGen(value: Option[ArithNode], branchLable: String, targetAddr: Option[Long]): String = {
  // generate code for return, if targetAddr is defined, instruction at targetAddr
  // Note that return is imprecise, we cannot ensure that the return instruction is at targetAddr, just the return in C
  // In the future we may add support for this by modifying the generated asm code
  val appendTryRun = s"""
       |.global ${branchLable}\n\t
       |${branchLable}:\n\t
                   """.stripMargin
  branchMapTryRun += ((branchLable + "Entry") -> appendTryRun)
  if (targetAddr.isDefined && AdvancedRelocation.relocate) {
    SMTCode.smtCode += s". = 0x${targetAddr.get.toHexString};\n"
    SMTCode.smtCode += s".customtext.${targetAddr.get.toHexString} : { *(.customtext.${targetAddr.get.toHexString})}\n"
    val append =
      s"""
         |.section .customtext.${targetAddr.get.toHexString},"ax",@progbits\n\t
                 """.stripMargin + appendTryRun

    branchMap += ((branchLable + "Entry") -> append)
  }

  s"""
     |__asm__ __volatile__ (
     |    ".global ${branchLable}Entry\\n\\t"
     |    "${branchLable}Entry:\\n\\t"
     |);
     | return (${if (value.isDefined) codeGenInternal(value.get) else ""});
  """.stripMargin
}

def BreakGen(branchLabel: String, targetAddr: Option[Long]): String = {
  // generate code for break, if targetAddr is defined, instruction at targetAddr
  // Note that break is imprecise, we cannot ensure that the break instruction is at targetAddr, just the break in C
  // In the future we may add support for this by modifying the generated asm code
  val appendTryRun =
    s"""
       |.global ${branchLabel}_jump\n\t
       |${branchLabel}_jump:\n\t
                  """.stripMargin
  if (targetAddr.isDefined && AdvancedRelocation.relocate) {
    SMTCode.smtCode += s". = 0x${targetAddr.get.toHexString};\n"
    SMTCode.smtCode += s".customtext.${targetAddr.get.toHexString} : { *(.customtext.${targetAddr.get.toHexString})}\n"

    val append =
      s"""
         |.section .customtext.${targetAddr.get.toHexString},"ax",@progbits\n\t
                 """.stripMargin + appendTryRun
    branchMap += ((branchLabel + "Entry_jump") -> append)
  }
  branchMapTryRun += ((branchLabel + "Entry_jump") -> appendTryRun)

  s"""
     |__asm__ __volatile__ (
     |    ".global ${branchLabel}Entry_jump\\n\\t"
     |    "${branchLabel}Entry_jump:\\n\\t"
     |);
     | break;
  """.stripMargin
}


def AsmConditionGen(node: ConditionNode, str: String, str1: String, maybeLong: Option[Long]) = {
  if (MarchParameters.ISA == "x86_64") {
    AMD64AsmConditionGen(node, str, str1, maybeLong)
  } else if (MarchParameters.ISA == "riscv64") {
    RISCV64AsmConditionGen(node, str, str1, maybeLong)
  } else {
    throw new Exception("Unknown ISA" + MarchParameters.ISA)
  }
}
/*
def WhenTailfix(node: WhenNode) = {
  if (MarchParameters.ISA == "x86_64") {
    AMD64WhenTailfix(node)
  } else if (MarchParameters.ISA == "riscv64") {
    RISCV64WhenTailfix(node)
  } else {
    throw new Exception("Unknown ISA" + MarchParameters.ISA)
  }
}*/

def LoadParam(i: Int): String = {
  if (MarchParameters.ISA == "x86_64") {
    AMD64LoadParam(i)
  } else if (MarchParameters.ISA == "riscv64") {
    RISCV64LoadParam(i)
  } else {
    throw new Exception("Unknown ISA" + MarchParameters.ISA)
  }
}

def ReserveRegStr: String = {
  if (MarchParameters.ISA == "x86_64") {
    AMD64ReserveRegStr
  } else if (MarchParameters.ISA == "riscv64") {
    RISCV64ReserveRegStr
  } else {
    throw new Exception("Unknown ISA" + MarchParameters.ISA)
  }
}

def PushCode(i: Int): String = {
  if (MarchParameters.ISA == "x86_64") {
    AMD64PushCode(i)
  } else if (MarchParameters.ISA == "riscv64") {
    RISCV64PushCode(i)
  } else {
    throw new Exception("Unknown ISA" + MarchParameters.ISA)
  }
}

def LoadCallAddr(name: String) = {
  if (MarchParameters.ISA == "x86_64") {
    AMD64LoadCallAddr
  } else if (MarchParameters.ISA == "riscv64") {
    RISCV64LoadCallAddr(name)
  } else {
    throw new Exception("Unknown ISA" + MarchParameters.ISA)
  }
}

def RealCall(name: String) = {
  if (MarchParameters.ISA == "x86_64") {
    AMD64RealCall
  } else if (MarchParameters.ISA == "riscv64") {
    RISCV64RealCall(name)
  } else {
    throw new Exception("Unknown ISA" + MarchParameters.ISA)
  }
}

def SpRecover(pushCount: Int) = {
  if (MarchParameters.ISA == "x86_64") {
    AMD64SpRecover(pushCount)
  } else if (MarchParameters.ISA == "riscv64") {
    RISCV64SpRecover(pushCount)
  } else {
    throw new Exception("Unknown ISA" + MarchParameters.ISA)
  }
}

def MFence = {
  if (MarchParameters.ISA == "x86_64") {
    AMD64MFence
  } else if (MarchParameters.ISA == "riscv64") {
    RISCV64MFence
  } else {
    throw new Exception("Unknown ISA" + MarchParameters.ISA)
  }
}

def LoadNoLabel(node: LoadNode) = {
  if (MarchParameters.ISA == "x86_64") {
    AMD64LoadNoLabel(node)
  } else if (MarchParameters.ISA == "riscv64") {
    RISCV64LoadNoLabel(node)
  } else {
    throw new Exception("Unknown ISA" + MarchParameters.ISA)
  }
}

def LoadLabel(node: LoadNode, labelName: String) = {
  if (MarchParameters.ISA == "x86_64") {
    AMD64LoadLabel(node, labelName)
  } else if (MarchParameters.ISA == "riscv64") {
    RISCV64LoadLabel(node, labelName)
  } else {
    throw new Exception("Unknown ISA" + MarchParameters.ISA)
  }
}

def StoreNoLabel(node: StoreNode) = {
  if (MarchParameters.ISA == "x86_64") {
    AMD64StoreNoLabel(node)
  } else if (MarchParameters.ISA == "riscv64") {
    RISCV64StoreNoLabel(node)
  } else {
    throw new Exception("Unknown ISA" + MarchParameters.ISA)
  }
}

def StoreLabel(node: StoreNode, labelName: String) = {
  if (MarchParameters.ISA == "x86_64") {
    AMD64StoreLabel(node, labelName)
  } else if (MarchParameters.ISA == "riscv64") {
    RISCV64StoreLabel(node, labelName)
  } else {
    throw new Exception("Unknown ISA" + MarchParameters.ISA)
  }
}

def RDTSC = {
  if (MarchParameters.ISA == "x86_64") {
    AMD64RDTSC
  } else if (MarchParameters.ISA == "riscv64") {
    RISCV64RDTSC
  } else {
    throw new Exception("Unknown ISA" + MarchParameters.ISA)
  }
}

def FENCEI = {
  if (MarchParameters.ISA == "x86_64") {
    AMD64FENCEI
  } else if (MarchParameters.ISA == "riscv64") {
    RISCV64FENCEI
  } else {
    throw new Exception("Unknown ISA" + MarchParameters.ISA)
  }
}

def flush_bp_history = {
  if (MarchParameters.ISA == "x86_64") {
    AMD64FlushBpHistory
  } else if (MarchParameters.ISA == "riscv64") {
    RISCV64FlushBpHistory
  } else {
    throw new Exception("Unknown ISA" + MarchParameters.ISA)
  }
}

def MarchCallFix(): Long = {
  if (MarchParameters.ISA == "x86_64") {
    0
  } else if (MarchParameters.ISA == "riscv64") {
    -4
  } else {
    throw new Exception("Unknown ISA" + MarchParameters.ISA)
  }
}

def DCacheFlushGen(node: ValueNode, lineCount: Int): String = {
  if (MarchParameters.ISA == "x86_64") {
    AMD64DCacheFlush(node, lineCount)
  } else if (MarchParameters.ISA == "riscv64") {
    RISCV64DCacheFlush(node, lineCount)
  } else {
    throw new Exception("Unknown ISA" + MarchParameters.ISA)
  }
}

def DCacheFlushPtrGen(node: ValueNode): String = {
  if (MarchParameters.ISA == "x86_64") {
    AMD64DCacheFlushPtr(node)
  } else if (MarchParameters.ISA == "riscv64") {
    RISCV64DCacheFlushPtr(node)
  } else {
    throw new Exception("Unknown ISA" + MarchParameters.ISA)
  }
}

def Crc32ComputeGen(node: Crc32ComputeNode) = {
  if (MarchParameters.ISA == "x86_64") {
    AMD64Crc32ComputeGen(node)
  } else {
    throw new Exception("Unsupported ISA " + MarchParameters.ISA)
  }
}

def PtrLoadGen(ptr: ValueNode, dst: ValueNode): String = {
  if (MarchParameters.ISA == "x86_64") {
    AMD64PtrLoad(ptr, dst)
  } else if (MarchParameters.ISA == "riscv64") {
    RISCV64PtrLoad(ptr, dst)
  } else {
    throw new Exception("Unknown ISA" + MarchParameters.ISA)
  }
}

def SequentialComputingDelayGen(src: ValueNode, rep: Int): String = {
  // sequential instruction sequences
  if (MarchParameters.ISA == "x86_64") {
    AMD64SequentialComputingDelay(src, rep)
  } else if (MarchParameters.ISA == "riscv64") {
    RISCV64SequentialComputingDelay(src, rep)
  } else {
    throw new Exception("Unknown ISA" + MarchParameters.ISA)
  }
}

def MarchSpecificCode: String = {
  if (MarchParameters.ISA == "x86_64") {
    AMD64SpecificCode
  } else if (MarchParameters.ISA == "riscv64") {
    RISCV64SpecificCode
  } else {
    throw new Exception("Unknown ISA" + MarchParameters.ISA)
  }
}

def genInlineAsmCode(node: InlineAsmNode): String = {
  node.body.map{
    a => "\"" + a + "\\n\\t\""
  }.mkString("\n") +
    "\n: " + node.outputs.map{
      a => "\"" + a.constrain + "\"(" + codeGenInternal(a.value) + ")"
    }.mkString(", ") +
    "\n: " + node.inputs.map {
      a => "\"" + a.constrain + "\"(" + codeGenInternal(a.value) + ")"
    }.mkString(", ") +
    "\n: " + node.clobbers.map {
      a => "\"" + a + "\""
    }.mkString(", ")
}

def codeGen(node: ASTNode): String = {
  val start =
    s"""
       |#define _GNU_SOURCE
       |#include <stdio.h>
       |#include <pthread.h>
       |#include <stdbool.h>
       |#include <stdint.h>
       |#include <stdlib.h>
       |#include <unistd.h>
       |#include <fcntl.h>
       |#include <sys/uio.h>
       |#include <sys/types.h>
       |#include <sys/wait.h>
       |#include <sys/mman.h>
       |#include <stdatomic.h>
       |""".stripMargin + MarchSpecificCode
  val funcs =
    s"""
       |
       |void read_process_memory(pid_t pid, void *remote_addr, void *local_buffer, size_t size) {
       |    char path[32];
       |    snprintf(path, sizeof(path), "/proc/%d/mem", pid);
       |
       |    int mem_fd = open(path, O_RDONLY);
       |    if (mem_fd == -1) {
       |        perror("open");
       |        return;
       |    }
       |
       |    // Seek to the address
       |    if (lseek(mem_fd, (off_t)remote_addr, SEEK_SET) == -1) {
       |        perror("lseek");
       |        close(mem_fd);
       |        return;
       |    }
       |
       |    // Read memory into local buffer
       |    ssize_t bytes_read = read(mem_fd, local_buffer, size);
       |    if (bytes_read == -1) {
       |        perror("read");
       |    } else {
       |        printf("Read %zd bytes from process %d at %p\\n", bytes_read, pid, remote_addr);
       |    }
       |
       |    close(mem_fd);
       |}
       |void set_thread_affinity(int core_id) {
       |    cpu_set_t cpuset;
       |    CPU_ZERO(&cpuset);             // Clear CPU set
       |    CPU_SET(core_id, &cpuset);     // Add the desired CPU core to the set
       |
       |    // Bind the calling thread to the CPU set
       |    if (sched_setaffinity(0, sizeof(cpu_set_t), &cpuset) != 0) {
       |        perror("sched_setaffinity");
       |        exit(EXIT_FAILURE);
       |    }
       |    printf("Thread bound to CPU %d\\n", core_id);
       |}
       |${RDTSC}
       |${FENCEI}
       |""".stripMargin
  val gen = codeGenInternal(node)
  val macroDef = paddingMacro()
  val remoteCode = RemoteCode.getRemoteCode()
  start + funcs + "\n" + macroDef + "\n" + FuncDeclCode.getFuncDeclCode()  + "\n" + gen + remoteCode
}
def codeGenInternal(node: ASTNode): String = {
  node match {
    case node: PrimitiveNode => {
      val body = node.body.map(codeGenInternal).mkString("\n")
      val preamble = node.preamble.map(codeGenInternal).mkString("\n")

      RemoteCode.remoteCode += preamble

      body
    }
    case node: WorldNode => {
      // recursively generate code for the body
      val body = node.body.map(codeGenInternal).mkString("\n")

      if (node.typ == "Attacker" && node.zone.isDefined && node.zone.get != Sequential && FuncPlacement.funcName != "") {
        // we should generate a new function for this zone and this zone will launch a new thread/process
        val funcName = AllocUniqueName("attackerNonSequential")
        val scheduleCode = if (OnSameCore(node.zone.get)) {
          // schedule the thread to run on the same process
          s"set_thread_affinity(${MarchParameters.DefaultCore});"
        } else if (OnSMTCore(node.zone.get)) {
          // schedule the thread to run on a new process
          s"set_thread_affinity(${MarchParameters.SMTCore});"
        } else {
          // schedule the thread to run on a new process
          s"set_thread_affinity(${MarchParameters.DiffCore});"
        }
        RemoteCode.remoteCode += "void* " + funcName + "(void* arg) {\n" + scheduleCode + body + "\n}\n"
        // here we generate code that start the remote function
        FuncDeclCode.funcDeclCode += "void* " + funcName + "(void* arg);\n"
          s"""
           |{
           |  // launch remote function
           |  ${if (InSameProcess(node.zone.get)) startThread(funcName, node.pidVar.get.name) else startProcess(funcName, node.pidVar.get.name)}
           |}""".stripMargin
      } else {
        // no world level code generation, body code only
        body
      }
    }
    case node: PullToLocalNode => {
      // generate code for the pull to local statement
      val ret =
        s"""
           |read_process_memory(${node.pid.name}, &${node.remote.name}, &${node.local.name}, sizeof(${node.local.name}));
           |""".stripMargin
      ret
    }
    case node: ThreadJoinNode => {
      s"pthread_join(${node.pidVar.name}, NULL);\n"
    }
    case node: ProcessJoinNode => {
      s"waitpid(${node.pidVar.name}, NULL, 0);\n"
    }

    case node: YieldNode => {
      s"sched_yield();"
    }

    case node: TLBEntryUnmapNode => {
      s"munmap(&${node.src.name}, ${MarchParameters.PageSize});"
    }

    case node: TLBEntryPermissionChangeNode => {
      if (!UniqueNameAddrMap.contains(node.src.uniname)) {
        throw new Exception("Cannot find address for " + node.src.uniname)
      }
      val addr = UniqueNameAddrMap(node.src.uniname)
      s"mprotect((void *)${addr - addr % MarchParameters.PageSize}L, ${MarchParameters.PageSize}, ${node.perm.toProtString});"
    }
    case node: IndirectCallNode => {
      val funcParam = node.func.decl.parameters.map {
        a => CodeType2C(a.paramType)
      }.mkString(", ")
      val funcRet = CodeType2C(node.func.decl.ret)
      val args = node.args.map {
        a => a.asInstanceOf[ValueNode].name
      }.mkString(", ")
      val callNode = if (node.branch.isDefined) {
        val branchInst = node.branch.get.decl.body.asInstanceOf[ControlflowInst]
        val branchName = branchInst.uniname

        // add tryRun and build branchMap
        val branchLabel = branchName + "Entry"
        val appendTryRun = s"""
             |.global ${branchName}\n\t
             |${branchName}:\n\t
             """.stripMargin

        branchMapTryRun += (branchLabel -> appendTryRun)

        if (AdvancedRelocation.relocate && SATParamSet.contains(branchInst.uniname)) {
          if (FuncPlacement.funcFirst) {
            FuncPlacement.funcFirst = false
          } else {
            // handle placement rule
            val expect_off = UniqueNameAddrMap(branchInst.uniname) - FuncPlacement.funcBase
            val off = expect_off - Offset(FuncPlacement.funcName, branchName)
            if (off < 0) {
              throw new Exception("Placement rule cannot be satisfied where actual code size exceeded " + (-off) + "bytes at " + FuncPlacement.funcName + " target" + node.branch.get.name)
            }
            //if (off > 0 && node.padding.isEmpty) {
            //  throw new Exception("Placement rule cannot be satisfied without padding for " + FuncPlacement.funcName + " target" + node.branch.get.name)
            //}
            if (node.padding.isDefined) {
              PaddingManager.mapAppend(node.padding.get.name, if (off - PaddingMaxGap > 0) (off - PaddingMaxGap) else 0)
            }
            FuncPlacement.funcBase += off // patch base to reflect added offset
          }
          val append =
            s"""
               |.section .customtext.${UniqueNameAddrMap(branchInst.uniname).toHexString},"ax",@progbits\n\t
                                             """.stripMargin + appendTryRun
          branchMap += (branchLabel -> append)
        }

        val EntryLabelCode: String = s"""
             |__asm__ __volatile__ (
             |    ".global ${branchName}Entry\\n\\t"
             |    "${branchName}Entry:\\n\\t"
             |);\n\t
             |""".stripMargin

          // add SMTCode
          SMTCode.smtCode += s". = 0x${UniqueNameAddrMap(branchInst.uniname).toHexString};\n"
          SMTCode.smtCode += s".customtext.${UniqueNameAddrMap(branchInst.uniname).toHexString} : { *(.customtext.${UniqueNameAddrMap(branchInst.uniname).toHexString})}\n"

        val ret = EntryLabelCode + s"""
                                      |{
                                      |${funcRet} (*${node.uniqueVar})($funcParam) = ${node.func.name};
                                      |${node.uniqueVar}(${args});
                                      |}
                                      |""".stripMargin
        ret
      } else {
        s"""
           |{
           |${funcRet} (*${node.uniqueVar})($funcParam) = ${node.func.name};
           |${node.uniqueVar}(${args});
           |}
           |""".stripMargin
      }
      callNode
    }

    case node: DeclareNode => {
      // generate code for the declaration
      // TODO: labeled declaration, should generate inline asm/black magic C
      if (node.typ == CacheLine) {
        // just skip code generation for objects
        ""
      } else if (node.typ == Imm) {
        // just skip code generation for immediate values
        throw new Exception("Immediate value should not be declared in C")
        ""
      } else {
        val uniname = node.typ match {
          case types.UInt64 => node.body.asInstanceOf[UInt64].uniname
          case types.UInt32 => node.body.asInstanceOf[UInt32].uniname
          case types.UInt16 => node.body.asInstanceOf[UInt16].uniname
          case types.UInt8 => node.body.asInstanceOf[UInt8].uniname
          case types.Int64 => node.body.asInstanceOf[Int64].uniname
          case types.Int32 => node.body.asInstanceOf[Int32].uniname
          case types.Int16 => node.body.asInstanceOf[Int16].uniname
          case types.Int8 => node.body.asInstanceOf[Int8].uniname
          case types.Bool => node.body.asInstanceOf[Bool].uniname
          case types.SInt => node.body.asInstanceOf[SInt].uniname
          case types.UInt => node.body.asInstanceOf[UInt].uniname
          case _ => ""
        }
        
        //UniqueNameAddrMap.foreach{
          // print out keys
        //  case (k, v) => println(s"$k -> $v")
        //}
        if (UniqueNameAddrMap.contains(uniname)) {
          val addr = UniqueNameAddrMap(uniname)
          val section = sectionStringC(
            "rule" + addr.toHexString
          )
          val nodename = "rule" + addr.toHexString
          val decl = s"${section}${CodeType2C(node.typ)} ${node.name} = ${node.body};"
          SMTCode.smtCode += s". = 0x${addr.toHexString};\n"
          SMTCode.smtCode += s".customdata.${
            nodename
          } : { *(.customdata.${nodename})}\n"
          decl
        } else if (node.rule.isDefined) {
          // generate section code
          val addr = if (node.linkedObj.isDefined) {
            // use linkedObj property to lookup
            UniqueNameAddrMap(node.linkedObj.get.decl.body.match {
              case b: ControlflowInst => b.uniname
              case l: LoadInst => l.uniname
              case s: StoreInst => s.uniname
              case l: LabelNode => l.name
              case c: Cacheline => c.uniname
              case t: TLBEntry => t.uniname
            })
          } else {
            UniqueNameAddrMap(node.typ match {
              case types.UInt => node.body.asInstanceOf[UInt].uniname
              case types.SInt => node.body.asInstanceOf[Int32].uniname
              case types.UInt8 => node.body.asInstanceOf[UInt8].uniname
              case types.UInt16 => node.body.asInstanceOf[UInt16].uniname
              case types.UInt32 => node.body.asInstanceOf[UInt32].uniname
              case types.UInt64 => node.body.asInstanceOf[UInt64].uniname
              case types.Int8 => node.body.asInstanceOf[Int8].uniname
              case types.Int16 => node.body.asInstanceOf[Int16].uniname
              case types.Int32 => node.body.asInstanceOf[Int32].uniname
              case types.Int64 => node.body.asInstanceOf[Int64].uniname
              case types.Bool => node.body.asInstanceOf[Bool].uniname
              case types.Atomic => node.body.asInstanceOf[Atomic].uniname
              case _ => throw new Exception("Unknown type for DeclareNode " + node.typ)
            })
          }

          val section = sectionStringC(
            "rule" + addr.toHexString
          )
          val nodename = "rule" + addr.toHexString
          val decl = s"${section}${CodeType2C(node.typ)} ${node.name} = ${node.body};"
          SMTCode.smtCode += s". = 0x${addr.toHexString};\n"
          SMTCode.smtCode += s".customdata.${
            nodename
          } : { *(.customdata.${nodename})}\n"
          decl
        } else if (node.typ == types.Array) {
          val decl = s"${CodeType2C(node.body.asInstanceOf[HTArray].typ)} ${node.name}[${node.body.asInstanceOf[HTArray].size}];"
          decl
        } else {
          val decl = s"${CodeType2C(node.typ)} ${node.name} = ${node.body};"
          decl
        }
      }
    }

    case node: FlushDCachePtrNode => {
      DCacheFlushPtrGen(node.ptr)
    }

    case node: PtrLoadNode => {
      PtrLoadGen(node.ptr, node.dst)
    }

    case node: Crc32ComputeNode => {
      Crc32ComputeGen(node)
    }

    case node: FlushICacheNode => {
      labelOperation(node.inst, node.padding, node.uniname, "flush_icache();")
    }

    case node: FlushNode => {
      node.sets.map { a => s"${a.name} = 0;" }.mkString("\n")
    }
    case node: SyscallSwitchNode => {
      "getuid();\n"
    }
    case node: SleepSwitchNode => {
      s"usleep(${node.unit});\n"
    }
    case node: MainRetNode => {
      if (MarchParameters.ISA == "x86_64") {
        s"return ${node.code};"
      } else if (MarchParameters.ISA == "riscv64") {
        if (MarchParameters.IsFPGA) ""
        else s"nemu_signal(GOOD_TRAP);"
      } else {
        throw new Exception("Unknown ISA for MainRet" + MarchParameters.ISA)
      }
    }
    case node: DCacheFlushNode => {
      labelOperation(node.inst, node.padding, node.uniname, DCacheFlushGen(node.node, node.lineCount))
    }

    case node: SequentialComputingDelayNode => {
      SequentialComputingDelayGen(node.node, node.rep)
    }

    case node: ProbeNode => {
      val mfenceCode = MFence
      node.sets.map(a => s"${a.name} = 0;\n\t" + mfenceCode).mkString("\n")
    }
    case node: FunctionDeclNode => {
      val parameters = node.parameters.map(p => s"${CodeType2C(p.paramType)} ${p.name}").mkString(", ")
      if (node.body.isEmpty) {
        // empty function body, must be a pure decl
        val decl = s"${CodeType2C(node.ret)} ${node.name}($parameters);"
        decl
      } else {
        checkFunctionObjectGaps(node)

        // generate code for the function declaration

        val schedCode = if (node.name == "main") {
          val append = if (MarchParameters.ISA == "x86_64") {
            ""
          } else if (MarchParameters.ISA == "riscv64") {
            if (MarchParameters.IsFPGA) ""
            else
            s"""
               |nemu_signal(DISABLE_TIME_INTR);
               |nemu_signal(NOTIFY_PROFILER);
            """.stripMargin
          } else {
            throw new Exception("Unknown ISA for MainEntry" + MarchParameters.ISA)
          }
          s"set_thread_affinity(${MarchParameters.DefaultCore});" + append
        } else {
          ""
        }

        val simple = s"${CodeType2C(node.ret)} ${node.name}($parameters);"
        FuncDeclCode.funcDeclCode += simple + "\n"

        var settled: Int = 0
        if ((node.rule.isDefined || UniqueNameAddrMap.contains(node.name)) || (AdvancedRelocation.relocate && node.placementObjects.nonEmpty)) {
          // generate section code
          if (node.rule.isDefined || UniqueNameAddrMap.contains(node.name)) {
            // debug: print UniqueNameAddrMap
            UniqueNameAddrMap.map{a => println(s"${a._1} ${a._2.toHexString}")}
            val addr = UniqueNameAddrMap(node.name)
            val section = noinlineC() + sectionTextStringCNoAX(
              if (node.rule.isDefined) {
                if (node.rule.get.decl.name == "ruleFromAddr") "rule" + addr.toHexString
                else node.rule.get.decl.name
              } else {
                node.name
              }

            )
            val nodename = if (node.rule.isDefined) {
              if (node.rule.get.decl.name == "ruleFromAddr") "rule" + addr.toHexString
              else node.rule.get.decl.name
            } else {
              node.name
            }

            FuncPlacement.funcFirst = false
            FuncPlacement.funcBase = addr
            FuncPlacement.funcName = node.name


            val body = node.body.map(codeGenInternal).mkString("\n")

            FuncPlacement.funcName = ""

            val decl = s"${section}${CodeType2C(node.ret)} ${node.name}($parameters) {\n${schedCode}\n$body\n}"
            SMTCode.smtCode += s". = 0x${addr.toHexString};\n"
            SMTCode.smtCode += s".customtext.${nodename} : { *(.customtext.${nodename})}\n"
            decl
          } else if (AdvancedRelocation.relocate) {
            // placement has at least one constrain

            val nodename = (node.placementObjects(0).body match {
              case b: ControlflowInst => b.uniname
              case l: LoadInst => l.uniname
              case s: StoreInst => s.uniname
              case l: LabelNode => l.name
              case a: AsmBlock => a.uniname
              case _ => throw new Exception("Unknown object type within codeGen " + node.placementObjects(0).body)
            }) + "_func"

            val section = noinlineC() + sectionTextStringCNoAX(
              nodename
            )

            FuncPlacement.funcFirst = true // handled first placement rule with function body
            FuncPlacement.funcBase = node.placementObjects(0).body match {
              case b: ControlflowInst => UniqueNameAddrMap(b.uniname) - Offset(node.name, b.uniname)
              case l: LoadInst => UniqueNameAddrMap(l.uniname) - Offset(node.name, l.uniname)
              case s: StoreInst => UniqueNameAddrMap(s.uniname) - Offset(node.name, s.uniname)
              case l: LabelNode => UniqueNameAddrMap(l.name) - Offset(node.name, l.name)
              case a: AsmBlock => UniqueNameAddrMap(a.uniname) - Offset(node.name, a.uniname)
              case _ => throw new Exception("Unknown object type within codeGen " + node.placementObjects(0).body)
            }
            FuncPlacement.funcName = node.name

            SMTCode.smtCode +=
              s"""
                    . = 0x${(FuncPlacement.funcBase - PaddingMaxGap).toHexString};\n""".stripMargin
            SMTCode.smtCode += s".customtext.${nodename} : { *(.customtext.${nodename})}\n"

            val body = node.body.map(codeGenInternal).mkString("\n")
            FuncPlacement.funcFirst = false
            FuncPlacement.funcName = ""
            val decl = s"__attribute__((noinline)) $section${CodeType2C(node.ret)} ${node.name}($parameters) {\n${schedCode}\n$body\n}"
            decl
          } else {
            ""
          }
        } else {
          FuncPlacement.funcFirst = false
          FuncPlacement.funcName = node.name
          val body = node.body.map(codeGenInternal).mkString("\n")
          FuncPlacement.funcName = ""
          val decl = s"__attribute__((noinline)) ${CodeType2C(node.ret)} ${node.name}($parameters) {\n${schedCode}\n$body\n}"
          decl
        }
      }

    }
    case node: ReturnNode => {
      // generate code for the return statement
      val targetAddr = if (node.branch.isDefined) {
        val branchInst = node.branch.get.decl.body.asInstanceOf[ControlflowInst]
        val addr = UniqueNameAddrMap(branchInst.uniname)
        Some(addr)
      } else {
        None
      }
      if (AdvancedRelocation.relocate && node.branch.isDefined && SATParamSet.contains(node.branch.get.decl.body.asInstanceOf[ControlflowInst].uniname)) {
        if (FuncPlacement.funcFirst) {
          FuncPlacement.funcFirst = false
        } else {
          // handle placement rule
          val branchInst = node.branch.get.decl.body.asInstanceOf[ControlflowInst]
          val expect_off = UniqueNameAddrMap(branchInst.uniname) - FuncPlacement.funcBase
          val off = expect_off - Offset(FuncPlacement.funcName, branchInst.uniname)
          if (off < 0) {
            throw new Exception("Placement rule cannot be satisfied where actual code size exceeded " + (-off) + " bytes at " + FuncPlacement.funcName + " target" + node.branch.get.name + " branch name " + node.branch.get.decl.body.asInstanceOf[ControlflowInst].uniname)
          }
          //if (off > 0 && node.padding.isEmpty) {
          //  throw new Exception("Placement rule cannot be satisfied without padding for " + FuncPlacement.funcName + " target" + node.branch.get.name)
          //}
          PaddingManager.mapAppend(node.padding.get.name, if (off - PaddingMaxGap > 0) (off - PaddingMaxGap) else 0)
          FuncPlacement.funcBase += off // patch base to reflect added offset
        }
      }
      val branchName = if (node.branch.isDefined) {
        node.branch.get.decl.body.asInstanceOf[ControlflowInst].uniname
      } else {
        node.uniname
      }
      val ret = AsmReturnGen(node.value, branchName, targetAddr)
      ret
    }
    case node: WhenNode => {
      // generate code for the when statement
      val whenBody = if (node.branch.isDefined) {
        // generate label for C code

        val branchInst = node.branch.get.decl.body.asInstanceOf[ControlflowInst]
        val branchName = branchInst.uniname

        // add tryRun and build branchMap
        val branchLabel = branchName + "Entry"
        val appendTryRun =
          s"""
             |.global ${branchName}\n\t
             |${branchName}:\n\t
                         """.stripMargin

        branchMapTryRun += (branchLabel -> appendTryRun)


        if (AdvancedRelocation.relocate && SATParamSet.contains(branchInst.uniname)) {
          if (FuncPlacement.funcFirst) {
            FuncPlacement.funcFirst = false
          } else {
            // handle placement rule
            val expect_off = UniqueNameAddrMap(branchInst.uniname) - FuncPlacement.funcBase
            val off = expect_off - Offset(FuncPlacement.funcName, branchName)
            if (off < 0) {
              throw new Exception("Placement rule cannot be satisfied where actual code size exceeded " + (-off) + "bytes at " + FuncPlacement.funcName + " target" + node.branch.get.name)
            }
            //if (off > 0 && node.padding.isEmpty) {
            //  throw new Exception("Placement rule cannot be satisfied without padding for " + FuncPlacement.funcName + " target" + node.branch.get.name)
            //}
            if (node.padding.isDefined) {
              PaddingManager.mapAppend(node.padding.get.name, if (off - PaddingMaxGap > 0) (off - PaddingMaxGap) else 0)
            }
            FuncPlacement.funcBase += off // patch base to reflect added offset
          }

          val append =
            s"""
               |.section .customtext.${UniqueNameAddrMap(branchInst.uniname).toHexString},"ax",@progbits\n\t
                                   """.stripMargin + appendTryRun
          branchMap += (branchLabel -> append)
        }

        val EntryLabelCode =
          s"""
             |__asm__ __volatile__ (
             |    ".global ${branchName}Entry\\n\\t"
             |    "${branchName}Entry:\\n\\t"
             |);\n\t
        """.stripMargin

        // add SMTCode
        SMTCode.smtCode += s". = 0x${UniqueNameAddrMap(branchInst.uniname).toHexString};\n"
        SMTCode.smtCode += s".customtext.${UniqueNameAddrMap(branchInst.uniname).toHexString} : { *(.customtext.${UniqueNameAddrMap(branchInst.uniname).toHexString})}\n"

        val cond = codeGenInternal(node.condition)
        val body = node.body.map(codeGenInternal).mkString("\n")
        val decl = EntryLabelCode + s"if ($cond) {\n$body\n}"
        decl
      } else {
        val cond = codeGenInternal(node.condition)
        val body = node.body.map(codeGenInternal).mkString("\n")
        val decl = s"if ($cond) {\n$body\n}"
        decl
      }

      val otherwisebody = if (node.otherwiseBody.body.nonEmpty) {
        "else {" + codeGenInternal(node.otherwiseBody.body.head) + "}"
      } else {
        ""
      }

      val elsewhenbody = if (node.elseWhens.nonEmpty) {
        node.elseWhens.reverse.zipWithIndex.map {
          case (a, i) => {
            val genCode = codeGenInternal(a)
            "else " + genCode
          }
        }.reverse.mkString("\n")
      } else {
        ""
      }

      whenBody + elsewhenbody + otherwisebody
    }
    case node: ElseWhenNode => {
      // generate code for the else when statement
      val elsewhenBody = if (node.branch.isDefined) {
        // generate label for C code
        val branchInst = node.branch.get.decl.body.asInstanceOf[ControlflowInst]

        val branchName = branchInst.uniname

        // add tryRun and build branchMap
        val branchLabel: String = branchName + "Entry"
        val appendTryRun: String =
          s"""
             |.global ${branchName}\n\t
             |${branchName}:\n\t
                         """.stripMargin
        branchMapTryRun += (branchLabel -> appendTryRun)

        if (AdvancedRelocation.relocate && SATParamSet.contains(branchInst.uniname)) {
          if (FuncPlacement.funcFirst) {
            FuncPlacement.funcFirst = false
          } else {
            // handle placement rule
            val expect_off = UniqueNameAddrMap(branchInst.uniname) - FuncPlacement.funcBase
            val off = expect_off - Offset(FuncPlacement.funcName, branchName)
            if (off < 0) {
              throw new Exception("Placement rule cannot be satisfied where actual code size exceeded" + (-off) + "bytes at " + FuncPlacement.funcName + " target: " + node.branch.get.name)
            }
            //if (off > 0 && node.padding.isEmpty) {
            //  throw new Exception("Placement rule cannot be satisfied without padding for " + FuncPlacement.funcName + " target" + node.branch.get.name)
            //}
            if (node.padding.isDefined) {
              PaddingManager.mapAppend(node.padding.get.name, if (off - PaddingMaxGap > 0) (off - PaddingMaxGap) else 0)
            }
            FuncPlacement.funcBase += off // patch base to reflect added offset
          }

          val append: String = appendTryRun + s"""
               |.section .customtext.${UniqueNameAddrMap(branchInst.uniname).toHexString},"ax",@progbits\n\t
                                   """.stripMargin
            branchMap += (branchLabel -> append)

        }

        val EntryLabelCode =
          s"""
             |__asm__ __volatile__ (
             |    ".global ${branchName}Entry\\n\\t"
             |    "${branchName}Entry:\\n\\t"
             |);\n\t
        """.stripMargin

        // add SMTCode
        val addr = UniqueNameAddrMap(branchInst.uniname)
        SMTCode.smtCode += s". = 0x${addr.toHexString};\n"
        SMTCode.smtCode += s".customtext.${addr.toHexString} : { *(.customtext.${addr.toHexString})}\n"

        val cond = codeGenInternal(node.condition)
        val body = node.body.map(codeGenInternal).mkString("\n")
        val decl = EntryLabelCode + s"if ($cond) {\n$body\n}"
        decl
      } else {
        val cond = codeGenInternal(node.condition)
        val body = node.body.map(codeGenInternal).mkString("\n")
        val decl = s"if ($cond) {\n$body\n}"
        decl
      }
      elsewhenBody
    }

    case node: ConditionNode => {
      // generate code for the condition
      val decl = if (node.toperator.toperator == "self") {
        codeGenInternal(node.body.head)
      } else {
        val operands = node.body.map(codeGenInternal).mkString(" " + node.toperator.toperator + " ")
        s"$operands"
      }
      decl
    }
    case node: ValueNode => {
      // generate code for the value node
      if (node.decl.typ == types.ArrayElement) {
        node.name + "[" + codeGenInternal(node.decl.body.asInstanceOf[ArrayElementNode].idx) + "]"
      } else {
        node.name
      }
    }
    case node: CallNode => {
      // generate code for the call node
      if (node.inst.isDefined) {
        // generate label for C code
        val branchInst = node.inst.get.decl.body.asInstanceOf[ControlflowInst]
        val branchName = branchInst.uniname

        // add tryRun and build branchMap
        val branchLabel = branchName + "Entry"
        val appendTryRun =
          s"""
             |.global ${branchName}\n\t
             |${branchName}:\n\t
                         """.stripMargin

        branchMapTryRun += (branchLabel -> appendTryRun)

        if (AdvancedRelocation.relocate && SATParamSet.contains(branchInst.uniname)) {
          if (FuncPlacement.funcFirst) {
            FuncPlacement.funcFirst = false;
          } else {
            // handle placement rule
            val expect_off = UniqueNameAddrMap(branchInst.uniname) - FuncPlacement.funcBase
            val off = expect_off - Offset(FuncPlacement.funcName, branchName)
            if (off < 0) {
              throw new Exception("Placement rule cannot be satisfied where actual code size exceeded " + (-off) + " bytes at " + FuncPlacement.funcName + " target" + node.inst.get.name)
            }
            //if (off > 0 && node.padding.isEmpty) {
            //  throw new Exception("Placement rule cannot be satisfied without padding for " + FuncPlacement.funcName + " target" + node.inst.get.name)
            //}
            if (node.padding.isDefined) {
              PaddingManager.mapAppend(node.padding.get.name, if (off - PaddingMaxGap > 0) (off - PaddingMaxGap) else 0)
            }
            FuncPlacement.funcBase += off // patch base to reflect added offset
          }

          val append =
            s"""
               |.section .customtext.${UniqueNameAddrMap(branchInst.uniname).toHexString},"ax",@progbits\n\t
                                   """.stripMargin + appendTryRun
          branchMap += (branchLabel -> append)
        }

        val EntryLabelCode =
          s"""
             |__asm__ __volatile__ (
             |    ".global ${branchLabel}\\n\\t"
             |    "${branchLabel}:\\n\\t"
             |);\n\t
        """.stripMargin

        // add SMTCode
        SMTCode.smtCode += s". = 0x${(UniqueNameAddrMap(branchInst.uniname) + MarchCallFix()).toHexString};\n"
        SMTCode.smtCode += s".customtext.${UniqueNameAddrMap(branchInst.uniname).toHexString} : { *(.customtext.${UniqueNameAddrMap(branchInst.uniname).toHexString})}\n"

        val args = node.args.map(codeGenInternal).mkString(", ")
        EntryLabelCode + s"${node.func.name}($args);"
      } else {
        val args = node.args.map(codeGenInternal).mkString(", ")
        s"${node.func.name}($args);"
      }

    }
    case node: InlineAsmNode => {
      // generate code for the inline assembly node
      if (node.handle.isDefined && AdvancedRelocation.relocate) {
        // handle inline assembly with a handle
        val handle = node.handle.get.decl.body.asInstanceOf[AsmBlock]
        val asmName = handle.uniname


        // add tryRun and build branchMap
        val handleLabel = asmName

        if (AdvancedRelocation.relocate && SATParamSet.contains(asmName)) {
          if (FuncPlacement.funcFirst) {
            FuncPlacement.funcFirst = false
          } else {
            // handle placement rule
            val expect_off = UniqueNameAddrMap(asmName) - FuncPlacement.funcBase
            val off = expect_off - Offset(FuncPlacement.funcName, asmName)
            if (off < 0) {
              throw new Exception("Placement rule cannot be satisfied where actual code size exceeded " + (-off) + " bytes at " + FuncPlacement.funcName + " target" + asmName)
            }
            /*
            if (node.padding.isDefined) {
              PaddingManager.mapAppend(node.padding.get.name, if (off - PaddingMaxGap > 0) (off - PaddingMaxGap) else 0)
            }*/
            FuncPlacement.funcBase += off // patch base to reflect added offset
          }
        }
        val asmCode = genInlineAsmCode(node)

        val EntryLabelCode =
          s"""
             |__asm__ __volatile__ (
             |    ".section .customtext.${UniqueNameAddrMap(asmName).toHexString},\\"ax\\",@progbits\\n\\t"\n\t
             |    ".global ${handleLabel}\\n\\t"
             |    "${handleLabel}:\\n\\t"
             |    ${asmCode}
             |);\n\t
        """.stripMargin

        // add SMTCode
        SMTCode.smtCode += s". = 0x${UniqueNameAddrMap(asmName).toHexString};\n"
        SMTCode.smtCode += s".customtext.${UniqueNameAddrMap(asmName).toHexString} : { *(.customtext.${UniqueNameAddrMap(asmName).toHexString})}\n"

        EntryLabelCode
      } else {
        // without location info
        val asmCode = genInlineAsmCode(node)
        val label = if (node.handle.isDefined) node.handle.get.decl.body.asInstanceOf[AsmBlock].uniname else ""
        s"""
           |__asm__ __volatile__ (
           ${if (node.handle.isDefined)
          s"""
             |".global ${label}\\n\\t"
             |"${label}:\\n\\t"
             |""".stripMargin else ""}
           |  ${asmCode}
           |);\n\t
           |""".stripMargin
      }
    }
    case node: PlacementDeclNode => {
      // generate code for the placement declaration, but to SMTCode
      val param = node.parameters.map(p => s"${p.name} = ${CodeType2Z3(p.paramType)}('${p.name}') ").mkString("\n")
      val body = node.body.map(codeGenInternal).mkString(",")
      // TODO: extract results from node
      //
      //SMTCode.smtCode += Z3Prefix() + param + "\n" + Z3BodyPrefix() + body + Z3BodyPostfix() + Z3PostfixLinker()
      ""
    }
    case node: ObjectValueNode => {
      // generate code for the object value node
      node.name
    }
    case node: ArithNode => {
      val operands = node.body.map(codeGenInternal).mkString(" " + node.toperator.toperator + " ")
      s"($operands)"
    }
    case node: TimingNode => {
      // generate code for the timing node
      val decl = s"unsigned long ${node.name}_start, ${node.name}_end;\n" + s"${node.name}_start = rdtsc();\n"
      val body = node.body.map(codeGenInternal).mkString("\n")
      val post = s"\n${node.name}_end = rdtsc() - ${node.name}_start;\n" + s"${node.dst.name} = ${node.name}_end;"
      decl + body + post
    }
    case node: AssignNode => {
      // generate code for the assignment node
      val decl = s"${codeGenInternal(node.left)} = ${codeGenInternal(node.right)};"
      decl
    }
    case node: LoopNode => {
      if (node.branch.isDefined) {
        // generate inline asm rather than C
        val branch_name = node.branch.get.decl.body.asInstanceOf[ControlflowInst].uniname
        val targetAddr = Some(UniqueNameAddrMap(branch_name))

        // add tryRun and build branchMap
        val branchLabel = branch_name + "Entry"
        val appendTryRun =
          s"""
             |.global ${branch_name}\n\t
             |${branch_name}:\n\t
                         """.stripMargin

        branchMapTryRun += ((branchLabel) -> appendTryRun)

        // handle placement rule
        if (node.branch.isDefined) {
          val branchInst = node.branch.get.decl.body.asInstanceOf[ControlflowInst]
          if (AdvancedRelocation.relocate && SATParamSet.contains(branchInst.uniname)) {
            if (FuncPlacement.funcFirst) {
              FuncPlacement.funcFirst = false
            } else {
              // handle placement rule
              val addr = UniqueNameAddrMap(branchInst.uniname)
              val expect_off = addr - FuncPlacement.funcBase
              val off = expect_off - Offset(FuncPlacement.funcName, branch_name)
              if (off < 0) {
                throw new Exception("Placement rule cannot be satisfied where actual code size exceeded at " + FuncPlacement.funcName + " target" + node.branch.get.name)
              }
              //if (off > 0 && node.padding.isEmpty) {
              //  throw new Exception("Placement rule cannot be satisfied without padding for " + FuncPlacement.funcName + " target" + node.branch.get.name)
              //}
              if (node.padding.isDefined) {
                PaddingManager.mapAppend(node.padding.get.name, if (off - PaddingMaxGap > 0) (off - PaddingMaxGap) else 0)
              }
              FuncPlacement.funcBase += off // patch base to reflect added offset
            }
            val append =
              s"""
                 |.section .customtext.${targetAddr.get.toHexString},"ax",@progbits\n\t
                                               """.stripMargin + appendTryRun
            branchMap += (branchLabel -> append)
          }
        }

        val EntryLabelCode =
          s"""
             |__asm__ __volatile__ (
             |    ".global ${branch_name}Entry\\n\\t"
             |    "${branch_name}Entry:\\n\\t"
             |);\n\t
        """.stripMargin

        // add SMTCode
        SMTCode.smtCode += s". = 0x${targetAddr.get.toHexString};\n"
        SMTCode.smtCode += s".customtext.${targetAddr.get.toHexString} : { *(.customtext.${targetAddr.get.toHexString})}\n"

        val cond = codeGenInternal(node.condition)
        val body = node.body.map(codeGenInternal).mkString("\n")

        val decl = EntryLabelCode + s"while (${codeGenInternal(node.condition)}) {"
         decl + body + "}"
      } else {
        // generate code for the loop node
        val decl = s"while (${codeGenInternal(node.condition)}) {"
        val body = node.body.map(codeGenInternal).mkString("\n")
        val post = "}"
        decl + body + post
      }


    }
    case node: BreakNode => {
      val branchName = if (node.branch.isDefined) {
        node.branch.get.decl.body.asInstanceOf[ControlflowInst].uniname
      } else {
        node.uniname
      }
      val targetAddr = if (node.branch.isDefined) {
        val addr = UniqueNameAddrMap(node.branch.get.decl.body.asInstanceOf[ControlflowInst].uniname)
        Some(addr)
      } else {
        None
      }
      if (AdvancedRelocation.relocate && node.branch.isDefined && SATParamSet.contains(node.branch.get.decl.body.asInstanceOf[ControlflowInst].uniname)) {
        if (FuncPlacement.funcFirst) {
          FuncPlacement.funcFirst = false
        } else {
          // handle placement rule
          val addr = UniqueNameAddrMap(node.branch.get.decl.body.asInstanceOf[ControlflowInst].uniname)
          val expect_off = addr - FuncPlacement.funcBase
          val off = expect_off - Offset(FuncPlacement.funcName, node.branch.get.decl.body.asInstanceOf[ControlflowInst].uniname + "_jump")
          if (off < 0) {
            throw new Exception("Placement rule cannot be satisfied where actual code size exceeded at " + FuncPlacement.funcName + " target" + node.branch.get.name)
          }
          //if (off > 0 && node.padding.isEmpty) {
          //  throw new Exception("Placement rule cannot be satisfied without padding for " + FuncPlacement.funcName + " target" + node.branch.get.name)
          //}
          if (node.padding.isDefined) {
            PaddingManager.mapAppend(node.padding.get.name, if (off - PaddingMaxGap > 0) (off - PaddingMaxGap) else 0)
          }
          FuncPlacement.funcBase += off // patch base to reflect added offset
        }
      }
      val breakAsm = BreakGen(branchName, targetAddr)

      breakAsm
    }
    case node: FlushBPHistoryNode => {
      labelOperation(node.inst, node.padding, node.uniname, flush_bp_history + ";")
    }
    case node: PrintIntNode => {
      // generate code for the print int node
      s"printf(\"%llu\\n\", ${node.value.name});"
    }
    case node: PrintMultipleNode => {
      // generate code for the advanced print node
      s"""printf(\"${node.form}\", ${node.param.map(
        a => if (a.decl.typ == types.Imm) a.decl.body.asInstanceOf[Imm].expr else a.name
      ).mkString(", ")});"""
    }
    case node: MFenceNode => {
      labelOperation(node.inst, node.padding, node.uniname, MFence)
    }
    case node: LoadNode => {
      // generate code for the load node using amd64 asm
      if (node.inst.isEmpty || !AdvancedRelocation.relocate) {
        LoadNoLabel(node)
      } else {
        if (AdvancedRelocation.relocate && node.inst.isDefined && SATParamSet.contains(node.inst.get.decl.body.asInstanceOf[LoadInst].uniname)) {
          if (FuncPlacement.funcFirst) {
            FuncPlacement.funcFirst = false
          } else {
            // handle placement rule
            val loadInst = node.inst.get.decl.body.asInstanceOf[LoadInst]
            val addr = UniqueNameAddrMap(loadInst.uniname)
            val expect_off = addr - FuncPlacement.funcBase
            val off = expect_off - Offset(FuncPlacement.funcName, node.inst.get.decl.body.asInstanceOf[LoadInst].uniname)
            if (off < 0) {
              throw new Exception("Placement rule cannot be satisfied where actual code size exceeded at " + FuncPlacement.funcName + " " + "load" + node.inst.get.name + ", off=" + (-off).toHexString)
            }
            //if (off > 0 && node.padding.isEmpty) {
            //  throw new Exception("Placement rule cannot be satisfied without padding for " + FuncPlacement.funcName + " where " + "load" + node.inst.get.name)
            //}
            if (node.padding.isDefined) {
              PaddingManager.mapAppend(node.padding.get.name, if (off - PaddingMaxGap > 0) (off - PaddingMaxGap) else 0)
            }
            FuncPlacement.funcBase += off // patch base to reflect added offset
          }
        }
        // add global label to the load instruction
        val inst = node.inst.get
        val labelName = node.uniname
        val addr = UniqueNameAddrMap(inst.decl.body.asInstanceOf[LoadInst].uniname)
        SMTCode.smtCode += s". = 0x${addr.toHexString};\n"
        SMTCode.smtCode += s".customtext.${labelName} : { *(.customtext.${labelName})}\n"
        LoadLabel(node, labelName)
      }

    }
    case node: StoreNode => {
      // generate code for the store node using amd64 asm
      if (node.inst.isEmpty || !AdvancedRelocation.relocate) {
        StoreNoLabel(node)
      } else {
        if (AdvancedRelocation.relocate && node.inst.isDefined && SATParamSet.contains(node.inst.get.decl.body.asInstanceOf[StoreInst].uniname)) {
          if (FuncPlacement.funcFirst) {
            FuncPlacement.funcFirst = false
          } else {
            // handle placement rule
            val inst = node.inst.get.decl.body.asInstanceOf[StoreInst]
            val addr = UniqueNameAddrMap(inst.uniname)
            val expect_off = addr - FuncPlacement.funcBase
            val off = expect_off - Offset(FuncPlacement.funcName, node.inst.get.decl.body.asInstanceOf[StoreInst].uniname)
            if (off < 0) {
              throw new Exception("Placement rule cannot be satisfied where actual code size exceeded at " + FuncPlacement.funcName + " " + "store" + node.inst.get.name)
            }
            //if (off > 0 && node.padding.isEmpty) {
            //  throw new Exception("Placement rule cannot be satisfied without padding for " + FuncPlacement.funcName + " " + "store" + node.inst.get.name)
            //}
            if (node.padding.isDefined) {
              PaddingManager.mapAppend(node.padding.get.name, if (off - PaddingMaxGap > 0) (off - PaddingMaxGap) else 0)
            }
            FuncPlacement.funcBase += off // patch base to reflect added offset
          }
        }
        // add global label to the store instruction
        val inst = node.inst.get
        val labelName = node.uniname
        val addr = UniqueNameAddrMap(inst.decl.body.asInstanceOf[StoreInst].uniname)
        SMTCode.smtCode += s". = 0x${addr.toHexString};\n"
        SMTCode.smtCode += s".customtext.${labelName} : { *(.customtext.${labelName})}\n"
        StoreLabel(node, labelName)
      }

    }

    case node: ObjectDeclareNode => {
      // generate no code for the object declaration
      node.body match {
        case b: PaddingNode =>
          s"""
             |asm(
             |".rept %c0\\n\\t"
             |"nop\\n\\t"
             |".endr\\n\\t"
             |: : "i" (${b.name}COUNT) :
             |);
             |""".stripMargin
        case b: ExactPaddingNode =>
          s"""
             |asm(
             |".rept %c0\\n\\t"
             |"nop\\n\\t"
             |".endr\\n\\t"
             |: : "i" (${b.sz}) :
             |);
             |""".stripMargin
        case b: JmpNode => {
          if (b.inst.isDefined) {
            val branchInst = b.inst.get
            if (AdvancedRelocation.relocate && SATParamSet.contains(branchInst.uniname)) {
              if (FuncPlacement.funcFirst) {
                FuncPlacement.funcFirst = false
              } else {
                val addr = UniqueNameAddrMap(branchInst.uniname)
                val expect_off = addr - FuncPlacement.funcBase
                val off = expect_off - Offset(FuncPlacement.funcName, branchInst.uniname)
                if (off < 0) {
                  throw new Exception("Placement rule cannot be satisfied where actual code size exceeded at " + FuncPlacement.funcName + " " + branchInst.uniname)
                }
                if (off > 0) {
                  throw new Exception("Placement rule cannot be satisfied without padding for " + FuncPlacement.funcName + " " + branchInst.uniname)
                }
                if (b.padding.isDefined) {
                  PaddingManager.mapAppend(b.padding.get.name, if (off - PaddingMaxGap > 0) (off - PaddingMaxGap) else 0)
                }
                FuncPlacement.funcBase += off // patch base to reflect added offset
              }
            }
          }
          if (b.inst.isDefined && AdvancedRelocation.relocate) {
            // should generate label according to branch name & rulev
            val inst = b.inst.get
            val addr = UniqueNameAddrMap(inst.uniname)
            if (SATParamSet.contains(inst.uniname)) {
              SMTCode.smtCode += s". = 0x${addr.toHexString};\n"
              SMTCode.smtCode += s".customtext.${inst.uniname} : { *(.customtext.${inst.uniname})}\n"
            }
            s"""
               |asm(
               |    ".section .customtext.${inst.uniname},\\"ax\\",@progbits\\n\\t"
               |    ".global ${b.name}\\n\\t"
               |    "${b.name}:\\n\\t"
               |    ".global ${inst.uniname}\\n\\t"
               |    "${inst.uniname}:\\n\\t"
               |    "${if (MarchParameters.ISA == "x86_64") "jmp" else if (MarchParameters.ISA == "riscv64") "j" else throw new Exception("Unknown ISA" + MarchParameters.ISA)} ${b.target.name}\\n\\t");
            """.stripMargin
          } else {
            s"""
               |asm(".global ${b.name}\\n\\t"
               |    "${b.name}:\\n\\t"
               |    "${
              if (b.inst.isDefined) {
                ".global " + b.inst.get.uniname + "\\n\\t" + b.inst.get.uniname + ":\\n\\t"
              } else ""
            }"
               |    "${if (MarchParameters.ISA == "x86_64") "jmp" else if (MarchParameters.ISA == "riscv64") "j" else throw new Exception("Unknown ISA" + MarchParameters.ISA)} ${b.target.name}\\n\\t");
            """.stripMargin
          }
        }
        case b: LabelNode => {
          if (AdvancedRelocation.relocate && SATParamSet.contains(b.name)) {
            val addr = UniqueNameAddrMap(b.name)
            SMTCode.smtCode += s". = 0x${addr.toHexString};\n"
            SMTCode.smtCode += s".customtext.${b.name} : { *(.customtext.${b.name})}\n"
            if (FuncPlacement.funcFirst) {
              FuncPlacement.funcFirst = false
            } else {
              // handle placement rule
              val expect_off = addr - FuncPlacement.funcBase
              val off = expect_off - Offset(FuncPlacement.funcName, b.name)
              if (off < 0) {
                throw new Exception("Placement rule cannot be satisfied where actual code size exceeded at " + FuncPlacement.funcName + " " + b.name)
              }

              //if (off > 0 && b.padding.isEmpty) {
              //throw new Exception("Placement rule cannot be satisfied without padding for " + FuncPlacement.funcName + " " + b.name)
              //}
              if (b.padding.isDefined) {
                PaddingManager.mapAppend(b.padding.get.name, if (off - PaddingMaxGap > 0) (off - PaddingMaxGap) else 0)
              }

              FuncPlacement.funcBase += off // patch base to reflect added offset
            }
            s"""
               |asm(
               |    ".section .customtext.${b.name},\\"ax\\",@progbits\\n\\t"
               |    ".global ${b.name}\\n\\t"
               |    "${b.name}:\\n\\t"
               |);
            """.stripMargin
          } else {
            s"""
               |asm(".global ${b.name}\\n\\t"
               |    "${b.name}:\\n\\t");
            """.stripMargin
          }

        }
        case b: ControlflowInst => ""
        case b: LoadInst => ""
        case b: StoreInst => ""
        case b: AsmBlock => ""
        case b: Cacheline => ""
        case b: TLBEntry => ""
        case b: UInt64 => ""
        case b: UInt32 => ""
        case b: UInt16 => ""
        case b: UInt8 => ""
        case b: UInt => ""
        case b: Int64 => ""
        case b: Int32 => ""
        case b: Int16 => ""
        case b: Int8 => ""
        case b: SInt => ""
        case b: Bool => ""
        case _ => throw new Exception("Unknown object type within object_decls_filtered2 " + node.body)
      }
    }

    case _ =>
      throw new Exception(s"Unknown node: $node")
  }
}

def codeGenWithMap(node: ASTNode, map: Map[String, String]): String = {
  node match {
    case node: ConditionNode => {
      // generate code for the condition
      val decl = if (node.toperator.toperator == "self") {
        val name = codeGenInternal(node.body.head)
        if (map.contains(name)) {
          map(name)
        } else {
          name
        }
      } else {
        val operands = node.body.map(codeGenWithMap(_, map)).mkString(" " + node.toperator.toperator + " ")
        s"$operands"
      }
      decl
    }
    case node: ValueNode => {
      val name = node.name
      if (node.decl.typ == types.ArrayElement) {
        name + "[" + codeGenWithMap(node.decl.body.asInstanceOf[ArithNode], map) + "]"
      } else if (map.contains(name)) {
        map(name)
      } else {
        name
      }
    }
    case node: ArithNode => {
      val operands = node.body.map(codeGenWithMap(_, map)).mkString(" " + node.toperator.toperator + " ")
      s"($operands)"
    }
  }

}

def printSMT() = {
  println(SMTCode.getSMTCode())
}

enum OSs {
  case Linux
  case Windows
  case MacOS
}

enum Archs {
  case x86_64
  case aarch64
  case riscv64
}

object Platform {
  var currentOS = OSs.Linux
  var currentArch = Archs.x86_64
}