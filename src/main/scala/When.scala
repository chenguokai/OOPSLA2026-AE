package HT

import scala.quoted.ToExpr
import scala.quoted.*
import scala.annotation.targetName
import scala.collection.mutable.ListBuffer
import HT.Types.*
import HT.ASTNodes.*

//object HardwareDSL {

  
  // `when` function that accepts a Bool and captures the AST
def when_impl(cond: => ConditionNode, branch: Option[ObjectValueNode] = None)(block: => Any): WhenNodeBuilder = {
    if (branch.isDefined) {
      if (branch.get.decl.typ != types.BranchInst) {
        throw new Exception("Branch in when should be a BranchInst")
      }
      FuncAddrConstraintStack.push(branch.get.decl)
      // add to interesting Labels
      InterestingLabels.insert(branch.get.decl.body.asInstanceOf[ControlflowInst].uniname)
    }
    val orig_sp = GlobalStack.size()

    val condNode = cond
    GlobalStack.pop(GlobalStack.size() - orig_sp)

    val localPadding: Option[PaddingNode] = if (PaddingStack.size() > 0 && branch.isDefined) {
      val padding = PaddingStack.top()
      PaddingStack.clear()
      Some(padding)
    } else {
      // if no padding or no branch for this when, leave padding for next usage
      None
    }

    println("when orig_sp: " + orig_sp)
    val result = block
    println("when sp after evaluating block " + GlobalStack.size())

    // extract statement nodes from GlobalStack.stack
    val statements = GlobalStack.slice(orig_sp, GlobalStack.size())
    // pop GlobalStack.stack
    GlobalStack.pop(GlobalStack.size() - orig_sp)

    // Construct the initial WhenNode and wrap it in a WhenNodeBuilder
    //val ret = '{ new WhenNodeBuilder(WhenNode($cond, ${Expr.ofList(statements.map { Expr(_) })})) }


    val ret = new WhenNodeBuilder(WhenNode(AllocUniqueName("unlabeled_when"), condNode, statements.toList, branch, padding = localPadding))
    // push whenNode to GlobalStack.stack
    println("When push to GlobalStack.stack")
    println("when cond show" + condNode)
    //GlobalStack.stack = GlobalStack.stack :+ '{ WhenNode($cond, ${Expr.ofList(statements.map { Expr(_) })}) }
    GlobalStack.push(ret.whenNode)
    ret
  }

def If(cond: => ConditionNode)(block: => Any): WhenNodeBuilder = {
  when_impl(cond, None)(block)
}

def If(cond: => ConditionNode, branch: ObjectValueNode)(block: => Any): WhenNodeBuilder = {
  when_impl(cond, Some(branch))(block)
}

  class WhenNodeBuilder(val whenNode: WhenNode) {
    // Method to add `elsewhen` blocks to the current `whenNode`
    def elsewhen(cond: => ConditionNode, branch: Option[ObjectValueNode] = None)(block: => Any): WhenNodeBuilder = {
      if (branch.isDefined) {
        if (branch.get.decl.typ != types.BranchInst) {
          throw new Exception("Branch in when should be a BranchInst")
        }
        FuncAddrConstraintStack.push(branch.get.decl)
        // add to interesting Labels
        InterestingLabels.insert(branch.get.decl.body.asInstanceOf[ControlflowInst].uniname)
      }

      val orig_sp = GlobalStack.size()
      
      val condNode = cond
      GlobalStack.pop(GlobalStack.size() - orig_sp)

      val localPadding: Option[PaddingNode] = if (PaddingStack.size() > 0 && branch.isDefined) {
        val padding = PaddingStack.top()
        PaddingStack.clear()
        Some(padding)
      } else {
        // if no padding or no branch for this when, leave padding for next usage
        None
      }
      
      println("elsewhen orig_sp: " + orig_sp)
      val result = block
      println("elsewhen sp after evaluating block " + GlobalStack.size())

      // extract statement nodes from GlobalStack.stack
      val statements = GlobalStack.slice(orig_sp, GlobalStack.size())
      // pop GlobalStack.stack
      GlobalStack.pop(GlobalStack.size() - orig_sp)

      // Use pattern matching to unwrap the builder expression and add the ElseWhenNode to the WhenNodeBuilder
      val ret = new WhenNodeBuilder(whenNode.copy(elseWhens = whenNode.elseWhens :+ ElseWhenNode(condNode, statements.toList, branch, localPadding)))
      // pop tail from GlobalStack.stack
      GlobalStack.pop(1) 
      println("elsewhen after pop size of GlobalStack.stack: " + GlobalStack.size())
      GlobalStack.push(ret.whenNode) 
      ret
    }

    // Add `otherwise` method to set the otherwiseBody
    def Else(block: => Any): WhenNodeBuilder = {
      println("otherwise begins")
      val orig_sp = GlobalStack.size()
      println("otherwise current_sp: " + orig_sp)
      val result = block
      println("otherwise sp after evaluating block " + GlobalStack.size())

      // extract statement nodes from GlobalStack.stack
      val statements = GlobalStack.slice(orig_sp, GlobalStack.size())
      // pop GlobalStack.stack
      GlobalStack.pop(GlobalStack.size() - orig_sp)

      // Use pattern matching to unwrap the builder expression and add the `otherwiseBody` to WhenNodeBuilder
      val ret = new WhenNodeBuilder(whenNode.copy(otherwiseBody = OtherwiseNode(statements.toList)))

      // pop tail from GlobalStack.stack
      GlobalStack.pop(1) 
      println("otherwise after pop size of GlobalStack.stack: " + GlobalStack.size())
      //println("otherwise body" + statementsExpr)
      // push whenNode to GlobalStack.stack
      GlobalStack.push(ret.whenNode)
      println("otherwise ends")
      ret
    }
    // Build method to finalize the node construction
    def build: WhenNode = whenNode
  }
//}