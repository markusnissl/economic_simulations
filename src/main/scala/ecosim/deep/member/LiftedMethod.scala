package ecosim.deep.member

import ecosim.deep.IR
import ecosim.deep.IR.Predef._
import ecosim.deep.algo.Algo

/**
  * Contains the information of a lifted method
  * Due to current constraints in squid, you have to pass the class and implement mtd of your own.
  *
  * @param cls      of lifted method
  * @param body     algorithm of method
  * @param blocking if method is blocking or not
  * @param methodId unique id of the method inside a cls
  * @param R        return type of method
  * @tparam R type of return value
  */
abstract class LiftedMethod[R](val cls: IR.TopLevel.Clasz[_], val body: Algo[R], val blocking: Boolean, val methodId: Int)(implicit val R: CodeType[R]) {
  val mtd: cls.Method[R, cls.Scp]

  override def hashCode() = sym.hashCode()

  override def equals(that: Any) = that match {
    case that: LiftedMethod[_] => that.sym === sym
  }

  override def toString = s"${sym.asMethodSymbol.owner.name}.${sym.asMethodSymbol.name}"

  def sym: IR.MtdSymbol = mtd.symbol

}
