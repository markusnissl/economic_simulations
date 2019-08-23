package ecosim.deep.algo

import ecosim.deep.IR
import ecosim.deep.IR.Predef._
import squid.lib.MutVar

/**
  * bound if None, then method chaining, else value is bound to bound and then rest is executed with bounded value
  * @param bound variable to bound value to
  * @param value first algorithm, which returns the bound
  * @param rest second algorithm, which is executed with the bounded bound
  * @param V codetype of v
  * @tparam V type of the variable
  * @tparam A return value of Algo
  */
case class LetBinding[V, A: CodeType](bound: Option[Variable[V]], value: Algo[V], rest: Algo[A])(implicit val V: CodeType[V]) extends Algo[A]
{
  override def codegen: List[IR.Predef.OpenCode[Unit]] = {
    val met1 = value.codegen

    if (bound.isEmpty) {
      val met2 = rest.codegen
      met1 ::: met2
    } else {
      var bindingMut = Variable[MutVar[V]]
      var contained = false

      // If variable is already defined, update it, else generate a new one and save it
      val finding = AlgoInfo.varSavers.find(_.from == bound.get)
      if (finding.isDefined) {
        bindingMut = finding.get.to.asInstanceOf[Variable[MutVar[V]]]
        contained = true
      } else {
        val tmp = AlgoInfo.VarWrapper(bound.get, bindingMut)
        AlgoInfo.variables = tmp :: AlgoInfo.variables
        AlgoInfo.varSavers = tmp :: AlgoInfo.varSavers
      }

      val bindingMutFinal = bindingMut

      val met2 = code"""$bindingMutFinal := ((${AlgoInfo.returnValue}!).asInstanceOf[V]); ()"""
      AlgoInfo.stateGraph.append(AlgoInfo.EdgeInfo("LetBinding met2", AlgoInfo.CodeNodePos(AlgoInfo.posCounter), AlgoInfo.CodeNodePos(AlgoInfo.posCounter+1), met2))
      AlgoInfo.nextPos

      val boundValue = bound.get
      val met3 = (rest.codegen).map(x => x.subs(boundValue).~>(code"($bindingMutFinal!)"))

      // If variable is defined here, remove it for the outer block again, it was defined only for the sub-block
      if (!contained) {
        AlgoInfo.varSavers = AlgoInfo.varSavers.filter(_.from != bound.get)
      }

      met1 ::: List(met2) ::: met3
    }
  }
}
