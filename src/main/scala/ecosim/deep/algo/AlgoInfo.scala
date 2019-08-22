package ecosim.deep.algo

import ecosim.deep.IR.Predef._
import simulation.core.ResponseMessage
import squid.lib.MutVar

import scala.collection.mutable.ListBuffer

object AlgoInfo {
  val timeVar: Variable[MutVar[Int]] = Variable[MutVar[Int]]
  val positionVar: Variable[MutVar[Int]] = Variable[MutVar[Int]]
  val positionStack: Variable[ListBuffer[Int]] = Variable[ListBuffer[Int]]
  val returnValue: Variable[MutVar[Any]] = Variable[MutVar[Any]]
  val responseMessage: Variable[MutVar[ResponseMessage]] = Variable[MutVar[ResponseMessage]]

  //Rename to make clear that this is dag info
  var merger: ListBuffer[(Boolean, Boolean)] = ListBuffer()

  var variables: List[VarWrapper[_]] = List()
  var varSavers:List[VarWrapper[_]] = List[VarWrapper[_]]()

  val pushCurrent = code"$positionStack.prepend(($positionVar!))"
  val pushNext = code"$positionStack.prepend(($positionVar!) + 1)"

  def jump(offset: Int) = code"$positionVar := ($positionVar!) + ${Const(offset)}"

  val restorePosition = code"""$positionVar := ($positionStack.remove(0) - 1); ()"""

  case class VarWrapper[C](from: Variable[C], to: Variable[MutVar[C]])(implicit val A: CodeType[C])

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

