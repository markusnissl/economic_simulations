package ecosim.deep.algo

import ecosim.deep.IR
import ecosim.deep.IR.Predef._
import ecosim.deep.algo.AlgoInfo.VarWrapper
import squid.lib.MutVar

/**
  * bound if None, then method chaining, else value is bound to bound and then rest is executed with bounded value
  *
  * @param bound variable to bound value to
  * @param value first algorithm, which returns the bound
  * @param rest  second algorithm, which is executed with the bounded bound
  * @param V     codetype of v
  * @tparam V type of the variable
  * @tparam A return value of Algo
  */
case class LetBinding[V, A: CodeType, T:CodeType](bound: Option[Variable[V]], value: Algo[V], rest: Algo[A], mutVar:Boolean = false, c: CodeType[T] = codeTypeOf[Unit])(implicit val V: CodeType[V]) extends Algo[A] {



  override def codegen: Unit = {
    value.codegen

    if (bound.isEmpty) {
      rest.codegen
      ()
    } else {
      //We skip the varsavers part for the if, since like squid work, it will never be the case, that a variable is reused in letbinding atm
      if (mutVar) {
        val x = bound.get.asInstanceOf[Variable[MutVar[T]]]
        val met2 = code"""$x := ((${AlgoInfo.returnValue}!).asInstanceOf[T]); ()"""
        println(met2)
        AlgoInfo.variables = VarWrapper(null, x) :: AlgoInfo.variables
        AlgoInfo.stateGraph.append(AlgoInfo.EdgeInfo("LetBinding met2", AlgoInfo.CodeNodePos(AlgoInfo.posCounter), AlgoInfo.CodeNodePos(AlgoInfo.posCounter + 1), met2))
        AlgoInfo.nextPos()

        rest.codegen
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
        AlgoInfo.stateGraph.append(AlgoInfo.EdgeInfo("LetBinding met2", AlgoInfo.CodeNodePos(AlgoInfo.posCounter), AlgoInfo.CodeNodePos(AlgoInfo.posCounter + 1), met2))
        AlgoInfo.nextPos()

        rest.codegen

        // If variable is defined here, remove it for the outer block again, it was defined only for the sub-block
        if (!contained) {
          AlgoInfo.varSavers = AlgoInfo.varSavers.filter(_.from != bound.get)
        }
      }


    }
  }
}
