import HT.ASTUtils.printAST
import HT.{Attacker, Constrain, Control, ExactPadding, Func, GlobalPass, GlobalSolve, If, Jmp, Label, Load, MarchParameters, Mfence, Padding, PlaceLabel, SMTCode, Store, Timing, Victim, While, XiangShan2ndGenParam, a, applyIntel14thGenParam, applyXiangShan2ndGenParam, call, codeGen, given_Conversion_ValueNode_PlacementOperator, imm, outputGen, placement, printInt, printSMT, refo, refv, ret, tryRun}
import HT.CodeGen.*
import HT.StdLib.{Cacheline2Var, DCacheFlush, FlushBPHistory, MainRet, SequentialComputingDelay, SyscallSwitch, USleepSwitch}
import HT.Types.{Bool, Cacheline, ControlflowInst, LoadInst, SInt, StoreInst, UInt, UInt64, types}
import HT.ASTNodes.given_Conversion_ObjectValueNode_ArithNode
import HT.ASTNodes.given_Conversion_ValueNode_ArithNode
import HT.CachelineUtils.EvictSetFromL1DCacheline
import HT.Executors.pyStr2Result
import HT.Permissions.Permission.{AttackerPrivate, AttackerPublic, VictimPublic}
import HT.ASTNodes.given_Conversion_Int_ValueNode
import HT.ASTNodes.given_Conversion_Int_ArithNode
import HT.given_Conversion_Int_PlacementOperator
import HT.ASTNodes.given_Conversion_Long_ValueNode
import HT.given_Conversion_Long_PlacementOperator
import HT.*
import HT.observation.*
import HT.simrunners.XSRun
import HT.observation.AbstractionLayer.*

def test103body = {
  val startTime = System.currentTimeMillis()
  val ast = Victim {
    val attackstore = StoreInst()
    val attackload = LoadInst()
    val trainstore = StoreInst()
    val trainload = LoadInst()
    val load_store_share = UInt64(0)
    val store_diff = UInt64(0)
    val indirect_train = UInt64(0)

    val train = Func(Bool)() {
      val retval = Bool(true)
      val tmp = UInt64(0)
      val dst = UInt64(0)

      load_store_share := 0 // load into cache
      Mfence()

      Load(indirect_train, tmp)  // load 0x20000040 to tmp
      StoreRef(tmp, tmp, inst = trainstore)            // store to load_store_share
      Load(load_store_share, dst, inst = trainload) // laod from load_store_share

      ret(retval)
    }

    val indirect = UInt64(0, permission = AttackerPublic)
    Attacker() {
      val probe = Func(Bool)() {
        val retval = Bool(true)
        val tmp = UInt64(0)
        val dst = UInt64(0)
        Load(src = indirect, dst = tmp) // load addr from a long miss position
        StoreRef(src = tmp, dst = tmp, inst = attackstore)     // store to store_diff
        Load(src = load_store_share, dst = dst, inst = attackload) // load from load_store_share
        SequentialComputingDelay(src = dst, rep = (if (MarchParameters.MarchName == "Intel7thGen") 10 else 25)) //
        ret(retval)
      }
    }

    val main = Func(SInt)() {
      val cost = UInt64(0, permission = VictimPublic)
      val total = UInt64(0)
      val i = UInt(0)

      Attacker() {
        (0 until 3).map {
          i => {
            indirect := 0x20000000
            DCacheFlush(indirect, 1)
            call("probe")()
          }
        }
      }
      While(i < (if (MarchParameters.ISA == "x86_64") 100000 else 0x100)) {
        // train load violation predictor
        val j = UInt(0)
        While(j < (if (MarchParameters.MarchName == "Intel7thGen") 1024 else 5)) {
          indirect_train := 0x20000040
          DCacheFlush(indirect_train)
          Mfence()
          train();
          j := j + 1
        }

        load_store_share := 0 // move into cacheline
        // probe code will see some slow down because of delayed load
        Attacker() {
          indirect := 0x20000000 // set pointer to a cache miss address
          DCacheFlush(indirect, 1)
          Mfence()
          Timing(cost) {
            call("probe")()
          }
        }
        Control(){
          If (cost > 0) {
            If (cost < 1500) {
              total := total + cost
            }
          }
        }
        i := i + 1
      }

      printInt(total)

      MainRet(0)
    }

    val attack:Boolean = true
    val offset = if (attack) 0 else 0x100
    if (MarchParameters.ISA == "x86_64") {
      Constrain() {
        // base requirements
        AppendConstraint(store_diff.obj.saddr === 0x20000000)
        AppendConstraint(attackstore.saddr === attackload.saddr - 16)
        AppendConstraint(trainstore.saddr === trainload.saddr - 16)
        NextDLine(store_diff.obj, load_store_share.obj)
        AppendConstraint(load_store_share.obj.page === store_diff.obj.page)// same page
        NextDLine(load_store_share.obj, indirect.obj)
        NextDLine(indirect.obj, indirect_train.obj)
        // Load violation requirements 
        AppendConstraint(attackload.saddr === trainload.saddr + 4096L * 1024 * 1024 + offset)// 32MB
        
        // Better when align to some place
        AppendConstraint(trainload.saddr === 0x80000240L)
        
      }
    } else {
      Constrain() {
        // base requirements
        AppendConstraint(store_diff.obj.saddr === 0x20000000)
        NextDLine(store_diff.obj, load_store_share.obj)
        AppendConstraint(load_store_share.obj.page === store_diff.obj.page) // same page
        NextDLine(load_store_share.obj, indirect.obj)
        NextDLine(indirect.obj, indirect_train.obj)
        // Load violation requirements
        AppendConstraint(attackstore.saddr === 0x77A90CA8)
        AppendConstraint(trainstore.saddr === 0x59100000)
        AppendConstraint(attackload.saddr - offset === 0x77A90CBC)
        AppendConstraint(trainload.saddr === 0x59100014)
        // Better when align to some place
      }
    }
  }
  GlobalPass(ast)
  println("Post CodeGen Current Time cost: " + (System.currentTimeMillis() - startTime) + "ms")

  if (MarchParameters.ISA == "riscv64") {
    // do hardware test
    val vcd_path = XSRun(100000L, dumpEndCycle = Some(100000))
    // val vcd_path = "/mnt/ssd4t/home/xim-intel14/loadvio.vcd"

    println("Post XSRun Current Time cost: " + (System.currentTimeMillis() - startTime) + "ms")

    val train_load_pc = observation.getPC(refo("trainload"))
    val train_store_pc = observation.getPC(refo("trainstore"))

    val attack_load_pc = observation.getPC(refo("attackload"))
    val attack_store_pc = observation.getPC(refo("attackstore"))

    val parser = VCDParser(vcd_path.toString, XSIbufferSignalSet ++ XSRenameSignalSet ++ XSROBSignalSet ++ XSFTQSignalSet ++ XSStoreSetSignalSet)

    val ibufObj = new XSIBuffer(parser)
    val renameObj = new XSRename(parser)
    val robObj = new XSROB(parser)
    val ftqObj = new XSFTQ(parser)
    val ssObj = new XSStoreSet(parser)

    println(s"VCD path : $vcd_path, max time: ${parser.getMaxTime}")
    println(s"Post VCDParser Current Time cost: " + (System.currentTimeMillis() - startTime) + "ms")

    var trainFoldPC: Option[BigInt] = None
    var trainLoadFtqIdx: (BigInt, BigInt) = (0, 0)
    var trainLoadFtqOffset: BigInt = 0

    var trainStoreFoldPC: Option[BigInt] = None
    var trainStoreFtqIdx: (BigInt, BigInt) = (0, 0)
    var trainStoreFtqOffset: BigInt = 0

    val trainLoadInBufferStart = () => {
      // if we have recorded train load into ibuf
      ibufObj.PCWithinValid(GlobalCycle, train_load_pc) >= 0 && ibufObj.outCanAccept(GlobalCycle)
    }

    val trainStoreCommit = () => {
      robObj.commitHasFtq(GlobalCycle, trainStoreFtqIdx._1, trainStoreFtqIdx._2, trainStoreFtqOffset)
    }

    val trainStoreRedirect = () => {
      ftqObj.hasRedirect(GlobalCycle) && !ftqObj.redirectIsYoungerThan(GlobalCycle, trainStoreFtqIdx._1, trainStoreFtqIdx._2, trainStoreFtqOffset)
    }

    val trainStoreFinish = () => {
      // if the train store has been committed in ROB or flushed in FTQ
      trainStoreCommit() || trainStoreRedirect()
    }

    val trainStoreFinishMeta = () => {
      if (trainStoreCommit()) {
        s"Store committed in ROB"
      } else {
        s"Store flushed in FTQ"
      }
    }

    val trainLoadCommit = () => {
      robObj.commitHasFtq(GlobalCycle - 10, trainLoadFtqIdx._1, trainLoadFtqIdx._2, trainLoadFtqOffset)
    }

    val trainLoadRedirect = () => {
      ftqObj.hasRedirect(GlobalCycle - 10) && !ftqObj.redirectIsYoungerThan(GlobalCycle - 10, trainLoadFtqIdx._1, trainLoadFtqIdx._2, trainLoadFtqOffset)
    }

    val trainLoadFinish = () => {
      trainLoadCommit() || trainLoadRedirect()
    }

    val trainLoadFinishMeta = () => {
      if (trainLoadCommit()) {
        s"Load committed in ROB"
      } else {
        s"Load flushed in FTQ"
      }
    }

    val trainStoreInBufferStart = () => {
      ibufObj.PCWithinValid(GlobalCycle, train_store_pc) >= 0 && ibufObj.outCanAccept(GlobalCycle)
    }

    val trainLoadCommitted = () => {
      robObj.commitHasFtq(GlobalCycle, trainStoreFtqIdx._1, trainStoreFtqIdx._2, trainStoreFtqOffset) ||
        ftqObj.hasRedirect(GlobalCycle) && !ftqObj.redirectIsYoungerThan(GlobalCycle, trainStoreFtqIdx._1, trainStoreFtqIdx._2, trainStoreFtqOffset)
    }

    val storeSetStart = () => {
      // during the attack store execution process, wait table can be updated to record the load violation
      ssObj.hasUpdate(GlobalCycle) && ssObj.updateMatches(GlobalCycle, trainFoldPC.get, trainStoreFoldPC.get)
    }

    val attackLoadDelayStart = () => {
      renameObj.renameMatchPC(GlobalCycle, attack_load_pc) != -1
    }

    val attackLoadDelayMeta = () => {
      if (renameObj.renameWaitBit(GlobalCycle, renameObj.renameMatchPC(GlobalCycle, attack_load_pc))) {
        s"Attack load delayed at cycle $GlobalCycle"
      } else {
        s"Attack load not delayed at cycle $GlobalCycle"
      }
    }

    val trainLoadInBufferBreakMeta = () => {
      if (robObj.commitHasFtq(GlobalCycle, trainStoreFtqIdx._1, trainStoreFtqIdx._2, trainStoreFtqOffset)) {
        s"Store committed in ROB, end at $GlobalCycle"
      } else {
        s"Store flushed in FTQ, end at $GlobalCycle"
      }
    }
    var startCycle: Long = 0
    def nextCycleStoreSetStart(): Boolean = {
      GlobalCycle != startCycle && storeSetStart()
    }

    setGraphRange(95000, 97000)

    // step 1: check if the train load gets recorded into storeSet
    Range(trainStoreInBufferStart, trainStoreFinish, "train store in buffer") {
      entry {
        val v = ibufObj.PCWithinValid(GlobalCycle, train_store_pc)
        trainStoreFoldPC = Some(ibufObj.getFoldPC(GlobalCycle, v))
        trainStoreFtqIdx = ibufObj.getFTQPtr(GlobalCycle, v)
        trainStoreFtqOffset = ibufObj.getFTQOffset(GlobalCycle, v)
        // step 2: check if the train store gets recorded into storeSet
        rangeOnceUnless(trainLoadInBufferStart, trainLoadFinish, trainLoadCommitted, "train load in buffer") {
          entry {
            val v = ibufObj.PCWithinValid(GlobalCycle, train_load_pc)
            trainFoldPC = Some(ibufObj.getFoldPC(GlobalCycle, v))
            trainLoadFtqIdx = ibufObj.getFTQPtr(GlobalCycle, v)
            trainLoadFtqOffset = ibufObj.getFTQOffset(GlobalCycle, v)
            // step 3: check if the load-store pair gets recorded into waitTable
            OnNextOnceUnless(storeSetStart, trainLoadFinish, "wait table update") {
              entry {
                // step 4: check if there are any attack load gets delayed before next storeSet update
                startCycle = GlobalCycle
                OnNextOnceUnless(attackLoadDelayStart, nextCycleStoreSetStart, "attack load delay") {
                  entry {
                    EventLog(attackLoadDelayMeta())
                  }
                }
              }
              abort {
                EventLog(trainLoadFinishMeta())
              }
            }
          }
          exit {
            EventLog(trainLoadFinishMeta())
          }
          abort {
            EventLog(trainLoadInBufferBreakMeta())
          }
        }
      }
      exit {
        EventLog(trainStoreFinishMeta())
      }
    }

    println(s"Post Test Current Time cost: " + (System.currentTimeMillis() - startTime) + "ms")

    generateGraph()
  }
}

@main def TestLoadvio_XSRange = {
  applyXiangShan2ndGenParam()
  test103body
}

@main def TestLoadvio_IARange = {
  applyIntel14thGenParam()
  test103body
}

@main def TestLoadvio_IA7 = {
  applyIntel7thGenParam()
  test103body
}

@main def TestLoadvio_AMD = {
  applyAMDZen4Param() // not working, index gen from physical addrs
  test103body
}