package HT.CachelineUtils

import HT.ASTNodes.{DeclareNode, ObjectDeclareNode, ObjectValueNode, PlacementNode, given_Conversion_ValueNode_ArithNode}
import HT.Executors.pyStr2Result
import HT.{AllocUniqueName, /*BaseRule2Python,*/ a, imm, placement, ruleFromAddr}
import HT.Types.{Cacheline, types}
import HT.MarchParameters
import HT.Permissions.CurrentDefaultPermission
import HT.applyNameToRule

import scala.collection.mutable.ListBuffer

def EvictSetFromL1DCacheline(cacheline: ObjectValueNode, evictRule: PlacementNode, evictDupRule: PlacementNode): List[ObjectValueNode] = {
  // evict a set from a cacheline
  //val mask = 1 << set
  //cacheline & ~mask
  if (cacheline.decl.typ != types.CacheLine) {
    throw new Exception("Cannot generate eviction set from a non-cacheline type " + cacheline.decl.typ)
  }
  val line = cacheline.decl.body.asInstanceOf[Cacheline]
  //val pythonCode = BaseRule2Python(line.addr, evictRule)
  //println("EvictSetFromL1DCacheline Code: " + pythonCode)
  //val result = pyStr2Result(pythonCode)
  //println("EvictSetFromL1DCacheline Result: " + result)
  //val addr = result.split(" = ").last.trim.toLong
  //println("EvictSetFromL1DCacheline Addr: " + addr)


  val buffer: ListBuffer[ObjectValueNode] = ListBuffer()
  val lastUniname = AllocUniqueName("cacheline")
  val baseRule = placement(AllocUniqueName("localrule"))("orig" -> types.Addr) {
    a("orig") > imm(0)
  }
  var lastLine = Cacheline(baseRule, "evictFirst", lastUniname, true)
  val declNode = ObjectDeclareNode.apply_no_push(CurrentDefaultPermission, types.CacheLine, lastLine, "evictLine")
  applyNameToRule(evictRule, List(line.uniname, lastLine.uniname))
  buffer += ObjectValueNode(declNode, "evictLine")
  var prevName = lastLine.uniname
  for (i <- 1 until MarchParameters.L1DWay) {
    lastLine = Cacheline(baseRule, "evictNext", AllocUniqueName("cacheline"), true)
    val declNode = ObjectDeclareNode.apply_no_push(CurrentDefaultPermission, types.CacheLine, lastLine, "evictLine")
    applyNameToRule(evictDupRule, List(prevName, lastLine.uniname))
    buffer += ObjectValueNode(declNode, "evictLine")
    prevName = lastLine.uniname
    //buffer += lastLine
  }
  buffer.toList
}