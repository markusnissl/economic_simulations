package ecosim.deep.algo

import ecosim.deep.IR
import ecosim.deep.IR.Predef._

import scala.collection.mutable.ListBuffer

case class IfThenElse[A](cond: OpenCode[Boolean], ifBody: Algo[A], elseBody: Algo[A])(implicit val A: CodeType[A]) extends Algo[A] {

  /**
    * 1. Check if condition is fullfilled,
    * 2. If yes, run if part and jump then to end of else part
    * 3. If not jump to else start and run the code
    * @return a list of opencode, containing individual program steps
    */
  override def codegen: List[IR.Predef.OpenCode[Unit]] = {
    //Append for met1 before calling met2
    AlgoInfo.merger.append((true, false))

    var tmp = AlgoInfo.merger
    AlgoInfo.merger = ListBuffer()
    val met2_1 = ifBody.codegen
    val met2 = AlgoInfo.mergeCodes(met2_1, AlgoInfo.merger.toList)
    AlgoInfo.merger = tmp
    AlgoInfo.mergeMerger(met2)

    //Append for metInner before calling met3
    AlgoInfo.merger.append((true, false))

    tmp = AlgoInfo.merger
    AlgoInfo.merger = ListBuffer()
    val met3_1 = elseBody.codegen
    val met3 = AlgoInfo.mergeCodes(met3_1, AlgoInfo.merger.toList)
    AlgoInfo.merger = tmp
    AlgoInfo.mergeMerger(met3)

    val met1 = code"""if(!$cond) {${AlgoInfo.jump(met2.length + 1)}}"""
    val metInner = code"""${AlgoInfo.jump(met3.length)}; ()"""

    List(met1) ::: met2 ::: List(metInner) ::: met3
  }

}
