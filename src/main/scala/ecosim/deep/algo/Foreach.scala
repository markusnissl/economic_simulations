package ecosim.deep.algo

import ecosim.deep.IR
import ecosim.deep.IR.Predef._
import squid.lib.MutVar

import scala.collection.mutable.ListBuffer

case class Foreach[E, R: CodeType](ls: OpenCode[List[E]], variable: Variable[E], f: Algo[R])(implicit val E: CodeType[E]) extends Algo[Unit] {

  override def codegen: List[IR.Predef.OpenCode[Unit]] = {
    val iter = Variable[Iterator[E]]
    val iterMut = Variable[MutVar[Iterator[E]]]
    val listValMut = Variable[MutVar[E]]

    AlgoInfo.variables = AlgoInfo.VarWrapper(iter, iterMut) :: AlgoInfo.variables
    AlgoInfo.variables = AlgoInfo.VarWrapper(variable, listValMut) :: AlgoInfo.variables

    //Merger of f2 has to be done before calling createCode of f3!!!
    AlgoInfo.merger.append((true, true))
    AlgoInfo.merger.append((false, false))

    val f1 = code"""$iterMut := $ls.iterator; ()"""


    val tmp = AlgoInfo.merger

    // sub-merging here required because of f3.length access to jump to correct position
    AlgoInfo.merger = ListBuffer()
    val f3_0 = AlgoInfo.restorePosition
    val f3_1 = f.codegen ::: List(f3_0)
    AlgoInfo.merger.append((true, false)) //f3_0
    val f3 = AlgoInfo.mergeCodes(f3_1, AlgoInfo.merger.toList)

    AlgoInfo.merger = tmp
    AlgoInfo.mergeMerger(f3)

    val f2 = code"""if($iter.hasNext) {${AlgoInfo.pushCurrent}; $listValMut := $iter.next;} else {${AlgoInfo.positionVar} := (${AlgoInfo.positionVar}!) + ${Const(f3.length)};}"""


    (List(f1, f2) ::: f3).map(x => x.subs(iter).~>(code"($iterMut!)")).map(x => x.subs(variable).~>(code"($listValMut!)"))
  }

}
