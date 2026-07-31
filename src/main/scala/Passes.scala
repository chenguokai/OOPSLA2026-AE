package HT

import HT.ASTNodes.WorldNode
import HT.ASTUtils.{filterNonVictimWorldCode, printAST}
import HT.{SMTCode, codeGen}
import HT.CodeGen.getLinker
import HT.Types.ObjectUsedSet

def GlobalPass(ast: WorldNode) = {
  printAST(ast)
  GlobalSolve()

  tryRun(codeGen(ast))
  println("NEXT BEGINS C Code")
  val finalCode = codeGen(ast)
  val linkerCode = getLinker(SMTCode.getSMTCode())
  outputGen(finalCode, linkerCode)
  println("Next begins constraints")
  println(printConstrains)
  if (ObjectUsedSet.nonEmpty) {
    // print remaining objects
    println("Warning: Remaining unused objects:")
    ObjectUsedSet.foreach(obj => println(s"  $obj"))
    println("You may want to check your code for unused variables.")
  }
}

def GlobalPassNoAttacker(ast: WorldNode) = {
  // trim non-victim world code
  val newast = filterNonVictimWorldCode(ast)
  if (!newast.isDefined) {
    throw new Exception("No victim world code found")
  }
  val processast = newast.get
  GlobalSolve()
  ControlAttackerInterestingLabels.clear()
  printAST(processast)
  tryRun(codeGen(processast))
  println("NEXT BEGINS C Code")
  val finalCode = codeGen(processast)
  val linkerCode = getLinker(SMTCode.getSMTCode())
  outputGen(finalCode, linkerCode)
  println("Next begins constraints")
  println(printConstrains)
}