package ecosim.deep.algo

import ecosim.deep.IR.Predef._
import ecosim.deep.member.ResponseMessage
import squid.lib.MutVar

import scala.collection.mutable.ListBuffer

/**
  * This object contains shared data between the algo codegens
  * so that it can be accessed from every algo subclass
  */
object AlgoInfo {

  /**
    * Helper class to save mappings between a variable introduced to a mutable variable, which is used in
    * the program. This is necessary, since it is not possible to define var variable references in squid
    * at the moment, thus we have to use the MutVar Wrapper to change the variable afterwards
    * @param from original variable defined by Algo
    * @param to new mutation variable created as replacement for original one
    * @param A code type of original variable
    * @tparam C type of original variable
    */
  case class VarWrapper[C](from: Variable[C], to: Variable[MutVar[C]])(implicit val A: CodeType[C])

  /**
    * Variable containing the current execution time.
    * Need to be increased, if some waiting has to be done after
    * finishing the current step
    */
  val timeVar: Variable[MutVar[Int]] = Variable[MutVar[Int]]

  /**
    * Variable containing the current position of the program.
    * It needs to be modified if the next step is not the next
    * program fragment, but a jump to somewhere else
    */
  val positionVar: Variable[MutVar[Int]] = Variable[MutVar[Int]]

  /**
    * This stack is used to save a position into it, so that it can be used to jump
    * to it in a later part. For example, to jump back after a method call
    */
  val positionStack: Variable[ListBuffer[Int]] = Variable[ListBuffer[Int]]

  /**
    * This variables is used as a register to store the return value of a call.
    */
  val returnValue: Variable[MutVar[Any]] = Variable[MutVar[Any]]

  /**
    * This variable is used to save data about the response message, if a blocking call is made
    */
  val responseMessage: Variable[MutVar[ResponseMessage]] = Variable[MutVar[ResponseMessage]]

  /**
    * List, saving all variables, which should be defined at the beginning
    */
  var variables: List[VarWrapper[_]] = List()
  /**
    * List, saving all variables already defined, so that a redefinement of the variable
    * is not necessary inside the used fragment.
    */
  var varSavers:List[VarWrapper[_]] = List[VarWrapper[_]]()

  /**
    * Helper, for pushing the current position on the stack
    */
  val pushCurrent = code"$positionStack.prepend(($positionVar!))"
  /**
    * Helper, for pushing the next position on the stack
    */
  val pushNext = code"$positionStack.prepend(($positionVar!) + 1)"

  /**
    * Helper, for jumping relativly to a different position
    * @param offset relative jumping offset
    * @return Code containing the position modification
    */
  def jump(offset: Int) = code"$positionVar := ($positionVar!) + ${Const(offset)}"

  /**
    * Helper, for restoring the position in the stack
    */
  val restorePosition = code"""$positionVar := ($positionStack.remove(0) - 1); ()"""


  /*The following code, will change when implementing a new state machine, therefore code is not commented yet*/

  //Rename to make clear that this is dag info
  var merger: ListBuffer[(Boolean, Boolean)] = ListBuffer()

  def mergeCodes(com: List[OpenCode[Unit]], mergeInfo: List[(Boolean, Boolean)]): List[OpenCode[Unit]] = {
    if (mergeInfo.isEmpty) {
      com
    } else if (mergeInfo.tail.isEmpty) {
      com
    } else {
      val current = mergeInfo.head
      val next = mergeInfo.tail.head

      if (current._2 && next._1) {
        val newMergeInfo = (current._1, next._2) :: mergeInfo.tail.tail
        val a = com.head
        val b = com.tail.head
        val comNew = (code"$a; $b") :: com.tail.tail
        mergeCodes(comNew, newMergeInfo)
      } else {
        com.head :: mergeCodes(com.tail, mergeInfo.tail)
      }
    }
  }

  /**
    * Note:
    * Not allowed to set true or false to beginning or ending, otherwise if jumps would be wrong and not working
    */
  def mergeMerger(com: List[OpenCode[Unit]]): Unit = {
    com.foreach(x => merger.append((false, false)))
  }
}

