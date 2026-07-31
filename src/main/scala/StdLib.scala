package HT.StdLib

import HT.ASTNodes.*
import HT.{GlobalStack, GlobalVarStack, PlacementEntityStack, PlacementOperator}
import HT.Permissions.{CurrentDefaultPermission, Permission}
import HT.Types.*
import HT.Types.types.CacheLine
import HT.*
import HT.given_Conversion_Int_PlacementOperator
import HT.ASTNodes.given_Conversion_ValueNode_ArithNode
import HT.ASTNodes.given_Conversion_Int_ValueNode
import HT.ASTNodes.given_Conversion_Int_ArithNode

import sourcecode.Name


def type2instance(init: Any, typ: types, perm: Option[Permission] = None, name: String): ValueBaseTypes = {
  val initStr_a = init.match {
    case i: Int => i.toString
    case i: String => i
    case i: Boolean => i.toString
    case _ => throw new Exception("Unsupported type " + init)
  }
  val initStr: String = if (typ == types.Bool) {
    val modified: String = if (initStr_a == "0") {
      "false"
    } else if (initStr_a == "1") {
      "true"
    } else {
      initStr_a
    }
    modified
  } else {
    initStr_a
  }
  val ret = typ match {
    case types.Bool => Bool.instance(initStr, perm, name)
    //case types.Int =>
    case types.Addr => Addr.instance(initStr, perm, name)
    case types.SInt => SInt.instance(initStr, perm, name)
    case types.Int8 => Int8.instance(initStr, perm, name)
    case types.Int16 => Int16.instance(initStr, perm, name)
    case types.Int32 => Int32.instance(initStr, perm, name)
    case types.Int64 => Int64.instance(initStr, perm, name)
    case types.UInt => UInt.instance(initStr, perm, name)
    case types.UInt8 => UInt8.instance(initStr, perm, name)
    case types.UInt16 => UInt16.instance(initStr, perm, name)
    case types.UInt32 => UInt32.instance(initStr, perm, name)
    case types.UInt64 => UInt64.instance(initStr, perm, name)

    case _ => throw new Exception("Unsupported type " + typ)
  }
  ret
}

// Support for CacheLine
def Cacheline2Var(line: ObjectValueNode, name: String, typ: types, init: Any, perm: Option[Permission] = None, rule: Option[PlacementNode] = None): ValueNode = {
  // bind the line to a variable
  // TODO: link the line to the variable
  if (line.decl.typ != CacheLine) {
    throw new Exception("Cannot generate variable from a non-cacheline type " + line.decl.typ)
  }
  val body = line.decl.body.asInstanceOf[Cacheline]
  //val body = line.body.asInstanceOf[Cacheline]
  if (perm.isEmpty) {
    val decl = DeclareNode(CurrentDefaultPermission, typ, type2instance(init, typ, perm, name), name, Some(line), Some(body.rule), true)
    GlobalStack.push(decl)
    GlobalVarStack.push(decl)
    ValueNode(decl, name)
  } else {
    val decl = DeclareNode(perm.get, typ, type2instance(init, typ, perm, name), name, Some(line), Some(body.rule), true)
    GlobalStack.push(decl)
    GlobalVarStack.push(decl)
    ValueNode(decl, name)
  }
}

def TLBEntry2Var(entry: ObjectValueNode, name: String, typ: types, init: Any, perm: Option[Permission] = None, rule: Option[PlacementNode] = None): ValueNode = {
  // bind the entry to a variable
  if (entry.decl.typ != types.TLBEntry) {
    throw new Exception("Cannot generate variable from a non-TLBEntry type " + entry.decl.typ)
  }
  val body = entry.decl.body.asInstanceOf[TLBEntry]
  if (perm.isEmpty) {
    val decl = DeclareNode(CurrentDefaultPermission, typ, type2instance(init, typ, perm, name), name, Some(entry), Some(body.rule), true)
    GlobalStack.push(decl)
    GlobalVarStack.push(decl)
    ValueNode(decl, name)
  } else {
    val decl = DeclareNode(perm.get, typ, type2instance(init, typ, perm, name), name, Some(entry), Some(body.rule), true)
    GlobalStack.push(decl)
    GlobalVarStack.push(decl)
    ValueNode(decl, name)
  }
}

def PagePermissionChange(src: => PlacementOperator, perm: TLBPermission) = {
  // should call with a page object
  val orig_sp = PlacementEntityStack.size()
  if (src.op != "page") {
    throw new Exception("TLBEntryPermissionChange should be called with a page object")
  }
  PlacementEntityStack.pop(PlacementEntityStack.size() - orig_sp) // accepted a page object, should consume its expressions
  val check = TLBEntryPermissionChangeNode(src, perm)
  GlobalStack.push(check)
}

def TLBEntryUnmap(src: ValueNode) = {
  // should call with a variable associated with a TLB entry
  if (src.decl.linkedObj.isEmpty || src.decl.linkedObj.get.decl.typ != types.TLBEntry) {
    throw new Exception("TLBEntryUnmap should be called with a TLB entry variable")
  }
  val unmap = TLBEntryUnmapNode(src)
  GlobalStack.push(unmap)
}

// Given eviction set, flush cache line at the call site
def FlushL1DByEvictionSet(sets: List[ValueNode]) = {
  val flush = FlushNode(sets)
  GlobalStack.push(flush)
  flush
}

def ProbeL1DByEvictionSet(sets: List[ValueNode]) = {
  val probe = ProbeNode(sets)
  GlobalStack.push(probe)
  probe
}

def FlushBPHistory(inst: Option[ObjectValueNode] = None) = {
  val (padding, uniname) = PrepareInstHandle(inst)
  val flush = FlushBPHistoryNode(inst, padding, uniname)
  GlobalStack.push(flush)
  flush
}

def SyscallSwitch() = {
  GlobalStack.push(SyscallSwitchNode())
}

def USleepSwitch(unit: Long) = {
  GlobalStack.push(SleepSwitchNode(unit))
}

def Yield() = {
  GlobalStack.push(YieldNode())
}

def MainRet(code: Int) = {
  GlobalStack.push(MainRetNode(code))
}

def DCacheFlush(node: ValueNode, lineCount: Int = 1, inst: Option[ObjectValueNode] = None) = {
  val (padding, uniname) = PrepareInstHandle(inst)
  val flush = DCacheFlushNode(node, lineCount, inst, padding, uniname)
  GlobalStack.push(flush)
}

def SequentialComputingDelay(src: ValueNode, rep: Int = 1) = {
  val delay = SequentialComputingDelayNode(src, rep)
  GlobalStack.push(delay)
}

def Var2Ptr(src: ValueNode, perm: Option[Permission] = None, rule: Option[PlacementNode] = None)(implicit name: Name): ValueNode = {
  // permission change is not enforced for pointers
  // users should never be able to change data from a pointer
  Ptr(src, perm, name.value, rule)
  refv(name.value)
}

def DCachePtrFlush(ptr: ValueNode) = {
  if (ptr.decl.typ != types.Ptr) {
    throw new Exception(s"Cannot flush cache from a non-pointer ptr" + ptr.name)
  }
  GlobalStack.push(FlushDCachePtrNode(ptr))
}

def PtrLoad(ptr: ValueNode, dst: ValueNode) = {
  if (ptr.decl.typ != types.Ptr) {
    throw new Exception(s"Cannot load from a non-pointer ptr" + ptr.name)
  }
  if (dst.decl.typ == types.Ptr) {
    throw new Exception(s"Cannot write to a ptr type " + ptr.name)
  }
  GlobalStack.push(PtrLoadNode(ptr, dst))
}

def Crc32Compute(src: List[ValueNode], key: List[ValueNode]) = {
  if (src.size != key.size) {
    throw new Exception(s"Crc32Compute src and key size mismatch: " + src.size + s" and " + key.size)
  }
  GlobalStack.push(Crc32ComputeNode(src, key))
}

def ICacheFlush(inst: Option[ObjectValueNode] = None) = {
  val (padding, uniname) = PrepareInstHandle(inst)
  GlobalStack.push(FlushICacheNode(inst, padding, uniname))
}

def InlineAsm_impl(body: List[String], outputs: List[AsmOperand], inputs: List[AsmOperand], clobbers: List[String], handle: Option[ObjectValueNode]) = {
  // create an InlineAsm node
  if (handle.isDefined) {
    InterestingLabels.insert(handle.get.decl.body.asInstanceOf[AsmBlock].uniname)
    FuncAddrConstraintStack.push(handle.get.decl)
  }
  
  if (handle.isDefined) {
    val inline = InlineAsmNode(body, outputs, inputs, clobbers, handle)
    
    GlobalStack.push(inline)
    PaddingStack.clear()
  } else {
    val inline = InlineAsmNode(body, outputs, inputs, clobbers, None)
    GlobalStack.push(inline)
  }
}

def InlineAsm(body: List[String], outputs: List[AsmOperand], inputs: List[AsmOperand], clobbers: List[String]) = {
  InlineAsm_impl(body, outputs, inputs, clobbers, None)
}

def InlineAsm(body: List[String], outputs: List[AsmOperand], inputs: List[AsmOperand], clobbers: List[String], handle: ObjectValueNode) = {
  InlineAsm_impl(body, outputs, inputs, clobbers, Some(handle))
}

var primitiveSet: Set[String] = Set()

// primitive support
def primitiveCall(name: String, preamble: => Any, callsite: => Any) = {
  val preambleList: List[ASTNode] = if (!primitiveSet.contains(name)) {
    // add primitve name to this set
    primitiveSet += name
    val orig_sp = GlobalStack.size()

    preamble

    val statements = GlobalStack.slice(orig_sp, GlobalStack.size())
    GlobalStack.pop(GlobalStack.size() - orig_sp)

    statements.toList
  } else {
    List()
  }
  // eval callsite
  val callsiteList = {
    val orig_sp = GlobalStack.size()
    callsite
    val statements = GlobalStack.slice(orig_sp, GlobalStack.size())
    statements.toList
  }
  val ret = PrimitiveNode(name, preambleList, callsiteList)
}

def examplePrimitive() = {
  primitiveCall(
    "example",
    {
      val primitive_example = Func(types.Bool)() {
        val a = Bool(init = true)
        ret(a)
      }
      Constrain() {
        refo("primitive_example").saddr === 0x10000000
      }
    },
    {
      call("primitive_example")()
    }
  )
}

def ICacheProbe(target: ObjectValueNode, offset: Int) = {
  primitiveCall(
    "ICacheProbe",
    {
      // prime+probe code
      /*
      val primerules = for (i <- 0 until MarchParameters.L1IWay) yield {
        placement(s"prime_rule$i")("a" -> types.Addr) {
          a("a") % 0x40 === 0
        }
      }*/

      (0 until MarchParameters.L1IWay).map {
        i =>
          val s = s"prime$i"
          val n: Name = Name(s)
          Func(types.Bool)() {
            val a = Bool(true)
            ret(a)
          }(n)
      }
      val primeList = (0 until MarchParameters.L1IWay).map {
        i => refo(s"prime$i")
      }.toList

      Constrain() {
        if (MarchParameters.ISA == "x86_64") {
          Intel14GInstrEvictionSet(target = target, eviction = primeList, offset = offset)
        } else if (MarchParameters.MarchName == "XiangShanNanhu") {
          refo("prime0").saddr === target.saddr + MarchParameters.L1ISet * MarchParameters.L1ILine + offset // this set will be replaced by phantom fetch
          refo("prime1").saddr === refo("prime0").saddr + MarchParameters.L1ILine * MarchParameters.L1ISet
          refo("prime2").saddr === refo("prime1").saddr + MarchParameters.L1ILine * MarchParameters.L1ISet
          refo("prime3").saddr === refo("prime2").saddr + MarchParameters.L1ILine * MarchParameters.L1ISet
        }
      }
    },
    {
      for (i <- 0 until MarchParameters.L1IWay) {
        call(s"prime$i")()
      }
    }
  )
}