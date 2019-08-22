package ecosim.deep.algo

import ecosim.deep.IR
import ecosim.deep.IR.Predef._
import squid.lib.MutVar

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
      AlgoInfo.merger.append((true, true))
      val boundValue = bound.get
      val met3 = (rest.codegen).map(x => x.subs(boundValue).~>(code"($bindingMutFinal!)"))

      if (!contained) {
        AlgoInfo.varSavers = AlgoInfo.varSavers.filter(_.from != bound.get)
      }

      met1 ::: List(met2) ::: met3
    }
  }
}
