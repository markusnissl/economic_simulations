package ecosim.deep.algo

import ecosim.deep.IR
import ecosim.deep.IR.Predef._
import squid.lib.MutVar

import scala.collection.mutable.ListBuffer

case class Foreach[E, R](ls: OpenCode[List[E]], variable: Variable[E], var f: Algo[R])(implicit val E: CodeType[E], implicit val R: CodeType[R]) extends Algo[Unit] {
  /**
    * 1. Get iterator to iterate over list
    * 2. As long as there is a next value, get next value and execute algo
    * 3. If there is no next value, jump to end of the code fragment
    *
    * This code adds a iter variable and a list variable as needed initialization to the variables list.
    *
    * @return a list of opencode, containing individual program steps
    */
  override def codegen: List[IR.Predef.OpenCode[Unit]] = {
    val iter = Variable[Iterator[E]]
    val iterMut = Variable[MutVar[Iterator[E]]]
    val listValMut = Variable[MutVar[E]]

    AlgoInfo.variables = AlgoInfo.VarWrapper(iter, iterMut) :: AlgoInfo.variables
    AlgoInfo.variables = AlgoInfo.VarWrapper(variable, listValMut) :: AlgoInfo.variables


    val f1 = code"""$iterMut := $ls.iterator; ()"""

    AlgoInfo.stateGraph.append(AlgoInfo.EdgeInfo("Foreach f1", AlgoInfo.CodeNodePos(AlgoInfo.posCounter), AlgoInfo.CodeNodePos(AlgoInfo.posCounter+1), f1))
    AlgoInfo.nextPos
    //Pos of f2 has to be saved before calling createCode of f3!!!
    val tmpPos = AlgoInfo.posCounter
    AlgoInfo.nextPos


    // sub-merging here required because of f3.length access to jump to correct position
    val f3_0 = AlgoInfo.restorePosition
    val f3 = f.codegen ::: List(f3_0)
    AlgoInfo.stateGraph.append(AlgoInfo.EdgeInfo("Foreach f3_0",AlgoInfo.CodeNodePos(AlgoInfo.posCounter), AlgoInfo.CodeNodePos(tmpPos), f3_0))
    AlgoInfo.nextPos

    val f2 = code"""if($iter.hasNext) {${AlgoInfo.pushCurrent}; $listValMut := $iter.next;} else {${AlgoInfo.jump(f3.length)};}"""
    AlgoInfo.stateGraph.append(AlgoInfo.EdgeInfo("Foreach f2 if",AlgoInfo.CodeNodePos(tmpPos), AlgoInfo.CodeNodePos(tmpPos+1), f2))
    AlgoInfo.stateGraph.append(AlgoInfo.EdgeInfo("Foreach f2 else",AlgoInfo.CodeNodePos(tmpPos), AlgoInfo.CodeNodePos(AlgoInfo.posCounter), f2))

    (List(f1, f2) ::: f3).map(x => x.subs(iter).~>(code"($iterMut!)")).map(x => x.subs(variable).~>(code"($listValMut!)"))
  }

}
