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
    val tmpPosMet1 = AlgoInfo.posCounter
    AlgoInfo.nextPos

    val met2 = ifBody.codegen

    //Append for metInner before calling met3
    val tmpPosMetInner = AlgoInfo.posCounter
    AlgoInfo.nextPos

    val met3 = elseBody.codegen

    val met1 = code"""if(!$cond) {${AlgoInfo.jump(met2.length + 1)}}"""
    AlgoInfo.stateGraph.append(AlgoInfo.EdgeInfo("IfThenElse met1 cond valid",AlgoInfo.CodeNodePos(tmpPosMet1), AlgoInfo.CodeNodePos(tmpPosMet1+1), met1))
    AlgoInfo.stateGraph.append(AlgoInfo.EdgeInfo("IfThenElse met1 cond invalid", AlgoInfo.CodeNodePos(tmpPosMet1), AlgoInfo.CodeNodePos(tmpPosMetInner+1), met1))

    val metInner = code"""${AlgoInfo.jump(met3.length)}; ()"""
    AlgoInfo.stateGraph.append(AlgoInfo.EdgeInfo("IfThenElse metInner", AlgoInfo.CodeNodePos(tmpPosMetInner), AlgoInfo.CodeNodePos(AlgoInfo.posCounter), metInner))

    List(met1) ::: met2 ::: List(metInner) ::: met3
  }

}
