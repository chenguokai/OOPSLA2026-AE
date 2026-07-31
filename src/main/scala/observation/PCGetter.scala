package HT.observation
import HT.ASTNodes.ObjectValueNode
import HT.{PythonHome, TmpFiles, UniqueNameAddrMap, patchScriptPrefix}

import sys.process.*

def getPC(ob: ObjectValueNode): Long = {
  val saddr = ob.saddr
  val name = saddr.uniname

  // val elf_name = TmpFiles.outDest.toString
  // val cmd = s"bash -c \"nm -C $elf_name | grep ' $name' | awk '{print $$1}'\""
  
  // val hexStr = cmd.!!.trim
  
  //val elf = java.lang.Long.parseLong(hexStr, 16)
  val ga = UniqueNameAddrMap.get(name).get
  //assert(elf == ga)
  ga
}

import play.api.libs.json._
import sys.process._



// Parse the first format
def parseFileNumbers(jsonStr: String): Option[Map[String, Seq[Long]]] = {
  Json.parse(jsonStr).validate[Map[String, Seq[Long]]].asOpt
}

// Parse the second format
case class NameAddress(name: String, addr: Long)

implicit val nameAddressReads: Reads[NameAddress] = Json.reads[NameAddress]

// Function to parse an array of NameAddress objects
def parseNameAddressList(jsonStr: String): Option[Seq[NameAddress]] = {
  Json.parse(jsonStr).validate[Seq[NameAddress]].asOpt
}

var nameAddress1: Seq[NameAddress] = Seq()
var nameAddress2: Seq[NameAddress] = Seq()
var fileNumbers: Map[String, Seq[Long]] = Map()

def ELFParseInit(elf_path: String) = {
  // call python script to generate 3 json files: 2 with format 2 and 1 with format 1
  val cmd = s"${PythonHome}/bin/python3 ${patchScriptPrefix}/gen.py $elf_path"
  // execute the command and get the output
  val jsonStr = sys.process.Process(cmd).!!
  // split 3 json parts
  val parts = jsonStr.split("\n\n").map(_.trim).filter(_.nonEmpty)
  assert(parts.length == 3, s"Expected 3 JSON parts, got ${parts.length}")
  nameAddress1 = parseNameAddressList(parts(0)).get
  nameAddress2 = parseNameAddressList(parts(1)).get
  fileNumbers = parseFileNumbers(parts(2)).get
  // println(s"nameAddress1 ${nameAddress1}")
  // println(s"nameAddress2 ${nameAddress2}")
}
//
//def getELFAddr(str: String): Long = {
//  // get address from ELF file
//  val ret = if (str.contains(":")) {
//    fileNumbers.get(str) match {
//      case Some(addrs) if addrs.nonEmpty => addrs.head
//      case _ => throw new Exception(s"Name $str not found in fileNumbers")
//    }
//  } else {
//    // either from nameAddress1 or nameAddress2
//    nameAddress1.find(_.name == str) match {
//      case Some(na) => na.addr
//      case None => nameAddress2.find(_.name == str) match {
//        case Some(na2) => na2.addr
//        case None => throw new Exception(s"Name $str not found in nameAddress lists")
//      }
//    }
//  }
//  ret
//}

def getELFAddr(str: String): Seq[Long] = {
  // get address from ELF file
  val ret = if (str.contains(":")) {
    fileNumbers.get(str) match {
      case Some(addrs) if addrs.nonEmpty => addrs
      case _ => throw new Exception(s"Name $str not found in fileNumbers")
    }
  } else {
    // either from nameAddress1 or nameAddress2
    (nameAddress1 ++ nameAddress2).collect { case NameAddress(`str`, addr) => addr } match {
      case addrs if addrs.nonEmpty => addrs
      case _ => throw new Exception(s"Name $str not found in nameAddress lists")
    }
  }
  ret
}

def getELFPos(addr: Long, depth:Int = 0): String = {
  // reverse lookup: nameAddress1, nameAddress2, or fileNumbers
  val nameOpt1 = nameAddress1.find(_.addr == addr).map(_.name)
  if (nameOpt1.isDefined) return nameOpt1.get
  val nameOpt2 = nameAddress2.find(_.addr == addr).map(_.name)
  if (nameOpt2.isDefined) return nameOpt2.get
  val nameOpt3 = fileNumbers.find { case (_, addrs) => addrs.contains(addr) }.map(_._1)
  if (nameOpt3.isDefined) return nameOpt3.get
  if (depth < 10) getELFPos(addr - 2, depth + 1)
  else s"Not found"
}