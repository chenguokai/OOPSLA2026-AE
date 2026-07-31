import HT.ASTUtils.printAST
import HT.{Attacker, Constrain, Control, ExactPadding, Func, GlobalPass, GlobalSolve, If, Jmp, Label, Load, MarchParameters, Mfence, Padding, PlaceLabel, SMTCode, Timing, Victim, While, XiangShan2ndGenParam, a, applyIntel14thGenParam, applyXiangShan2ndGenParam, call, codeGen, given_Conversion_ValueNode_PlacementOperator, imm, outputGen, placement, printInt, printSMT, refo, refv, ret, tryRun}
import HT.CodeGen.*
import HT.StdLib.{Cacheline2Var, DCacheFlush, DCachePtrFlush, FlushBPHistory, InlineAsm, MainRet, PtrLoad, SyscallSwitch, USleepSwitch, Var2Ptr}
import HT.Types.{AsmBlock, Bool, Cacheline, ControlflowInst, HTArray, SInt, UInt, UInt64, types}
import HT.ASTNodes.{AsmOperand, given_Conversion_Int_ArithNode, given_Conversion_Int_ValueNode, given_Conversion_Long_ValueNode, given_Conversion_ObjectValueNode_ArithNode, given_Conversion_ValueNode_ArithNode}
import HT.CachelineUtils.EvictSetFromL1DCacheline
import HT.Executors.pyStr2Result
import HT.Permissions.Permission.VictimPublic
import HT.given_Conversion_Int_PlacementOperator
import HT.given_Conversion_Long_PlacementOperator
import HT.*
import HT.RISCV64.ZicondSelAdd
import HT.observation.*
import HT.simrunners.XSRun
import HT.observation.AbstractionLayer.*

def test105body = {
  val startTime = System.currentTimeMillis()
  val zicond: Boolean = true
  val inlineAsm: Boolean = true
  val noRandom: Boolean = false
  val flushDCache: Boolean = false

  val totalIte = 1000

  val ast = Victim {
    val eval_handle = AsmBlock()
    val state = UInt64(1453)


    val my_rand = Func(UInt64)() {
      if (!noRandom) {
        val x = UInt64(0)
        x := state
        x := x ^ (x >> 12)
        x := x ^ (x << 25)
        x := x ^ (x >> 27)
        state := x
        state := state * 2685821657736338717L
        ret(state)
      } else {
        ret(state)
      }
    }

    val counter = UInt64(0)
    val aa = UInt64(1)
    val bb = UInt64(3)

    val test_start = Label()
    val test_end = Label()

    val main = Func(SInt)() {
      // main function, iteratively call eval
      val i = UInt64(0)

      val v = UInt64(0)

      val a = HTArray(totalIte, types.UInt64)

      While(i < totalIte) {
        my_rand()
        a.at(i) := state
        i := i + 1
      }

      if (flushDCache)
        DCacheFlush(a.at(0), 125)

      i := 0
      While (i < totalIte) {
        PlaceLabel(test_start)
        if (zicond) {
          // implementation with zicond
          if (inlineAsm) {
            InlineAsm(
              body = List(
                /*
                ".rept 32",
                "j 1f",
                "nop",
                "nop",
                "nop",
                "1:",
                ".endr",
                 */
                "andi t0, %1, 1",
                "czero.eqz t1, %2, t0",
                "czero.nez t2, %3, t0",
                "add t1, t1, t2",
                "add %0, %0, t1"),
              outputs = List(AsmOperand("+r", refv("counter"))),
              inputs = List(AsmOperand("r", a.at(i)), AsmOperand("r", aa), AsmOperand("r", bb)),
              clobbers = List("t0", "t1", "t2"),
              handle = eval_handle
            )
          } else {
            ZicondSelAdd(refv("counter"), a.at(i), aa, bb, eval_handle)
          }

        } else {
          // implementation with conditional branch
          InlineAsm(
            body = List(
              /*
              ".rept 32",
              "j 1f",
              "nop",
              "nop",
              "nop",
              "1:",
              ".endr",
               */
              "andi t0, %1, 1",
              "beqz t0, 0f",
              "add %0, %0, %2",
              "j 1f",
              "0:",
              "add %0, %0, %3",
              "1:"),
            outputs = List(AsmOperand("+r", counter)),
            inputs = List(AsmOperand("r", a.at(i)), AsmOperand("r", aa), AsmOperand("r", bb)),
            clobbers = List("t0"),
            handle = eval_handle
          )
        }
        PlaceLabel(test_end)
        i := i + 1
      }

      printInt(counter)

      MainRet(0)
    }

    if (MarchParameters.ISA == "riscv64") {
      Constrain() {
        AppendConstraint(test_start.saddr === (0x20000000L - 0x34))
        AppendConstraint(eval_handle.saddr === 0x20000000L)
        AppendConstraint(test_end.saddr === 0x20000012L)
      }
    }
  }
  GlobalPass(ast)
  println("Post CodeGen Current time cost: " + (System.currentTimeMillis() - startTime) + " ms")
  val vcd_path = XSRun(700000L, dumpStartCycle = Some(0), dumpEndCycle = None)
  // val vcd_path = if (zicond) "/home/xim-intel14/XS/build/2025-11-09@12:23:43.vcd" else "/home/xim-intel14/XS/build/2025-11-09@12:15:19.vcd"
  //GlobalIte = 0
  println(s"Post XSRun Current time cost: ${System.currentTimeMillis() - startTime} ms")
  val test_block_pc = observation.getPC(refo("test_start")) // + 0x100
  val test_block_ret_pc = observation.getPC(refo("test_end"))

  val capt = observation.examples.branch_notifier_signals
  val parser = VCDParser(vcd_path.toString, XSIbufferSignalSet ++ XSROBSignalSet ++ XSFTQSignalSet)

  val ibufObj = new XSIBuffer(parser)
  val robObj = new XSROB(parser)
  val ftqObj = new XSFTQ(parser)

  println(s"VCD path: $vcd_path, max time: ${parser.getMaxTime}")
  println(s"Post VCDParser Current time cost: ${System.currentTimeMillis() - startTime} ms")

  var redirectionCounter = 0 // between block leaves ibuf and ret block leaves ibuf
  var totalRedirectionCounter = 0

  var startCycle = 0
  var totalExecCycle = 0

  var blockFtqIdx: (BigInt, BigInt) = (0, 0)
  var blockFtqOffset: BigInt = 0

  var retBlockInIbuf: Boolean = false

  var blockCommitted: Int = 0x7fffffff

  val blockInIbufStart = () => {
    val cond = ibufObj.PCWithinValid(GlobalCycle, test_block_pc) >= 0 && ibufObj.outCanAccept(GlobalCycle)
    if (cond) {
      val v = ibufObj.PCWithinValid(GlobalCycle, test_block_pc)
      val blockFtqIdx = ibufObj.getFTQPtr(GlobalCycle, v)
      val blockFtqOffset = ibufObj.getFTQOffset(GlobalCycle, v)
      !(ftqObj.hasRedirect(GlobalCycle - 1) && ftqObj.redirectIsOlderThan(GlobalCycle - 1, blockFtqIdx._1, blockFtqIdx._2, blockFtqOffset)) &&
        !(ftqObj.hasRedirect(GlobalCycle) && ftqObj.redirectIsOlderThan(GlobalCycle, blockFtqIdx._1, blockFtqIdx._2, blockFtqOffset))
    } else {
      false
    }
  }

  val retInIbufStart = () => {
    ibufObj.PCWithinValid(GlobalCycle, test_block_ret_pc) >= 0 && ibufObj.outCanAccept(GlobalCycle)
  }

  val redirectBeforeBlock = () => {
    (blockCommitted == 0x7fffffff) && ftqObj.hasRedirect(GlobalCycle) && !ftqObj.redirectIsYoungerThan(GlobalCycle, blockFtqIdx._1, blockFtqIdx._2, blockFtqOffset)
  }

  val retRobCommit = () => {
    // if a younder commit than block, with ret block pc
    val v = robObj.getValidFtqs(GlobalCycle)
    val p = robObj.getValidPCs(GlobalCycle)
    (blockCommitted < GlobalCycle) && v.zip(p).exists { case (ptr, pc) =>
      pc == test_block_ret_pc && ftqObj.isOlderThan(blockFtqIdx._1, blockFtqIdx._2, blockFtqOffset, ptr._1, ptr._2, ptr._3)
    }
  }

  val redirectAfterBlock = () => {
    ftqObj.hasRedirect(GlobalCycle) && ftqObj.redirectIsYoungerThan(GlobalCycle, blockFtqIdx._1, blockFtqIdx._2, blockFtqOffset)
  }

  setGraphRange(75000, 77000)

  val blockEnd = () => {
    redirectBeforeBlock() || retRobCommit()
  }

  val blockRobCommit = () => {
    // if a commit goes past our block start, not precise because ROB compression
    robObj.commitIsYounger(GlobalCycle, blockFtqIdx._1, blockFtqIdx._2, blockFtqOffset)
  }

  val blockEndReason = () => {
    if (redirectBeforeBlock()) {
      "redirect before block"
    } else {
      "ret rob commit"
    } + s"total cycles: ${GlobalCycle - startCycle} redirection count: $redirectionCounter"
  }

  val blockInIbufMeta = () => {
    s"Block FTQ flag: ${blockFtqIdx._1} value: ${blockFtqIdx._2} offset: $blockFtqOffset at cycle: $GlobalCycle"
  }

  def blockCommit(): Boolean = {
    blockCommitted == 0x7fffffff && blockRobCommit()
  }

  def mainBlockCommit(): Boolean = {
    robObj.commitIsYounger(GlobalCycle, blockFtqIdx._1, blockFtqIdx._2, blockFtqOffset) && robObj.getValidPCs(GlobalCycle).exists {
      a => a >= 0x20000000  // 0x1fffffd4
    }
  }

  var totalWasted = 0

  var testStartTime = 0
  var firstTime = true

  var totalBeforeMainBlock = 0

  Range(blockInIbufStart, blockEnd, "block in exec") {
    entry {
      if (firstTime) {
        testStartTime = GlobalCycle
        firstTime = false
      }
      // record ftq idx and offset
      val v = ibufObj.PCWithinValid(GlobalCycle, test_block_pc)
      blockFtqIdx = ibufObj.getFTQPtr(GlobalCycle, v)
      blockFtqOffset = ibufObj.getFTQOffset(GlobalCycle, v)
      // record start cycle
      startCycle = GlobalCycle
      redirectionCounter = 0 // reset counter for part 1
      blockCommitted = 0x7fffffff // reset block committed flag
      EventLog(blockInIbufMeta())
      OnOnceUnless(blockCommit, redirectBeforeBlock, "prepare block exec") {
        entry {
          blockCommitted = GlobalCycle
          EventLog(s"Block committed at cycle: $GlobalCycle")
        }
      }
      if (blockCommitted != 0x7fffffff) {
        OnOnceUnless(mainBlockCommit, redirectBeforeBlock, "test block exec") {
          entry {
            EventLog(s"The main block committed at cycle: $GlobalCycle, time from fetch ${GlobalCycle - startCycle}")
            totalBeforeMainBlock += (GlobalCycle - startCycle)
          }
        }
      }
    }
    exit{
      // here we calculate the total cucle cost
      totalExecCycle += GlobalCycle - startCycle
      EventLog(s"Commit from start: ${GlobalCycle - startCycle}")
      if (redirectBeforeBlock()) {
        totalWasted += GlobalCycle - startCycle
        totalRedirectionCounter += 1
        EventLog("redirect before block")
      } else {
        EventLog("ret rob commit")
      }
    }
    /*
    inRange{
      check(redirectBeforeBlock, "redirection") {
        redirectionCounter += 1
      }
    }*/
  }

  println("Total execution cycles: " + totalExecCycle)
  println("Total redirection count: " + totalRedirectionCounter)
  println("Total wasted cycles: " + totalWasted)
  println("Total before main block commit cycles: " + totalBeforeMainBlock)
  println("Program execution cycles: " + (parser.getMaxTime - testStartTime))

  println(s"Post range Current time cost: ${System.currentTimeMillis() - startTime} ms")

  generateGraph()

}


@main def TestZicond_XS = {
  applyXiangShan2ndGenParam()
  test105body
}