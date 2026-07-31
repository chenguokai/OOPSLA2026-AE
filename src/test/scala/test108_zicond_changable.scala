import HT.ASTNodes.*
import HT.ASTUtils.printAST
import HT.CachelineUtils.EvictSetFromL1DCacheline
import HT.CodeGen.*
import HT.Executors.pyStr2Result
import HT.Permissions.Permission.VictimPublic
import HT.StdLib.*
import HT.Types.*
import HT.observation.*
import HT.observation.AbstractionLayer.*
import HT.simrunners.XSRun
import HT.*
import HT.ASTNodes.given_Conversion_ValueNode_ArithNode
import HT.ASTNodes.given_Conversion_Int_ValueNode
import HT.ASTNodes.given_Conversion_Long_ValueNode
import HT.ASTNodes.given_Conversion_Int_ArithNode
import HT.given_Conversion_Long_PlacementOperator

def test108body = {
  val startTime = System.currentTimeMillis()
  val zicond: Boolean = false

  val ast = Victim {
    val eval_handle = AsmBlock()
    val state = UInt64(1453)

    val my_rand = Func(UInt64)() {
      val x = UInt64(0)
      x := state
      x := x ^ (x >> 12)
      x := x ^ (x << 25)
      x := x ^ (x >> 27)
      state := x
      state := state * 2685821657736338717L
      ret(state)
    }

    val counter = UInt64(0)
    val aa = UInt64(1)
    val bb = UInt64(3)

    val eval = Func(Bool)("a" -> types.UInt64) {

      val retval = UInt64(0)

      if (zicond) {
        // implementation with zicond
        InlineAsm(
          body = List(
               "andi t0, %1, 1",
               "czero.eqz t1, %2, t0",
               "czero.nez t2, %3, t0",
               "add t1, t1, t2",
               "add %0, %0, t1"),
          outputs = List(AsmOperand("+r", refv("counter"))),
          inputs = List(AsmOperand("r", v("a")), AsmOperand("r", refv("aa")), AsmOperand("r", refv("bb"))),
          clobbers = List("t0", "t1", "t2"),
          handle = eval_handle
        )
      } else {
        // implementation with conditional branch
        InlineAsm(
          body = List("andi t0, %1, 1",
               "beqz t0, 0f",
               "add %0, %0, %2",
               "j 1f",
               "0:",
               "add %0, %0, %3",
               "1:"),
          outputs = List(AsmOperand("+r", counter)),
          inputs = List(AsmOperand("r", v("a")), AsmOperand("r", aa), AsmOperand("r", bb)),
          clobbers = List("t0"),
          handle = eval_handle
        )
      }

      ret(counter)
    }


    val eval_handle_ret = Label()

    val rate = 4

    val main = Func(SInt)() {
      // main function, iteratively call eval
      val i = UInt64(0)

      val v = UInt64(0)

      val tmp = UInt(0)

      While(i < 1000) {
        my_rand()
        If (state % 10 < rate) { // note: this would fail because the next H2P branch can learn the correlation
          tmp := 0
        } .Else {
          tmp := 1
        }
        eval(None, tmp)
        PlaceLabel(eval_handle_ret)
        i := i + 1
      }

      printInt(counter)

      MainRet(0)
    }

    if (MarchParameters.ISA == "riscv64") {
      Constrain() {
        AppendConstraint(refo("eval_handle").saddr === 0x20000000L)
        AppendConstraint(eval_handle_ret.saddr === 0x10000000L)
      }
    }
  }
  GlobalPass(ast)
  println("Post CodeGen Current time cost: " + (System.currentTimeMillis() - startTime) + " ms")
  val vcd_path = XSRun(700000L, dumpStartCycle = Some(0), dumpEndCycle = None)
  //val vcd_path = if (zicond) "/mnt/ssd4t/home/xim-intel14/zicond.vcd" else "/mnt/ssd4t/home/xim-intel14/baseline-br.vcd"
  //GlobalIte = 0
  println(s"Post XSRun Current time cost: ${System.currentTimeMillis() - startTime} ms")
  val test_block_pc = observation.getPC(refo("eval_handle"))
  val test_block_ret_pc = observation.getPC(refo("eval_handle_ret"))

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

  var blockCommitted: Boolean = false

  val blockInIbufStart = () => {
    ibufObj.PCWithinValid(GlobalCycle, test_block_pc) >= 0 && ibufObj.outCanAccept(GlobalCycle)
  }

  val retInIbufStart = () => {
    ibufObj.PCWithinValid(GlobalCycle, test_block_ret_pc) >= 0 && ibufObj.outCanAccept(GlobalCycle)
  }

  val redirectBeforeBlock = () => {
   !blockCommitted && ftqObj.hasRedirect(GlobalCycle) && !ftqObj.redirectIsYoungerThan(GlobalCycle, blockFtqIdx._1, blockFtqIdx._2, blockFtqOffset)
  }

  val retRobCommit = () => {
    // if a younder commit than block, with ret block pc
    val v = robObj.getValidFtqs(GlobalCycle)
    val p = robObj.getValidPCs(GlobalCycle)
    v.zip(p).exists { case (ptr, pc) =>
      pc == test_block_ret_pc && ftqObj.isOlderThan(blockFtqIdx._1, blockFtqIdx._2, blockFtqOffset, ptr._1, ptr._2, ptr._3)
    }
  }

  val redirectAfterBlock = () => {
    ftqObj.hasRedirect(GlobalCycle) && ftqObj.redirectIsYoungerThan(GlobalCycle, blockFtqIdx._1, blockFtqIdx._2, blockFtqOffset)
  }

  setGraphRange(75000, 78000)

  val blockEnd = () => {
    redirectBeforeBlock() || retRobCommit()
  }

  val blockRobCommit = () => {
    // if a commit with the same ftq ptr as the block
    robObj.commitHasFtq(GlobalCycle, blockFtqIdx._1, blockFtqIdx._2, blockFtqOffset)
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

  Range(blockInIbufStart, blockEnd, "block in exec") {
    entry {
      // record ftq idx and offset
      val v = ibufObj.PCWithinValid(GlobalCycle, test_block_pc)
      blockFtqIdx = ibufObj.getFTQPtr(GlobalCycle, v)
      blockFtqOffset = ibufObj.getFTQOffset(GlobalCycle, v)
      // record start cycle
      startCycle = GlobalCycle
      redirectionCounter = 0 // reset counter for part 1
      blockCommitted = false // reset block committed flag
      EventLog(blockInIbufMeta())
    }
    exit{
      // here we calculate the total cucle cost
      totalExecCycle += GlobalCycle - startCycle
      totalRedirectionCounter += redirectionCounter
      EventLog(blockEndReason())
    }
    inRange{
      check(redirectAfterBlock, "redirect after block") {
        redirectionCounter += 1
      }
    }
  }

  println("Total execution cycles: " + totalExecCycle)
  println("Total redirection count: " + totalRedirectionCounter)
  println("Program execution cycles: " + parser.getMaxTime)

  println(s"Post range Current time cost: ${System.currentTimeMillis() - startTime} ms")

  generateGraph()

}


@main def TestZicondRate_XS = {
  applyXiangShan2ndGenParam()
  test108body
}