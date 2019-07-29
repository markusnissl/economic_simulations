package ecosim
package deep

import IR.Predef._

case class Message[A,R](mtd: NonLocalMethod[A,R], arg: OpenCode[A])

sealed abstract class Algo[A](implicit tpe: CodeType[A])
case class Forever(body: Algo[Unit]) extends Algo[Unit]
case class Send[R](actorRef: OpenCode[runtime.Actor], msg: Message[_,R]) extends Algo[Unit]
case class Foreach[A: CodeType](ls: OpenCode[List[A]], f: Variable[A] => Algo[Unit]) extends Algo[Unit]
case class ScalaCode[A: CodeType](cde: OpenCode[A]) extends Algo[A]
case class LetBinding[A: CodeType, B: CodeType](bound: Variable[A], value: OpenCode[A], body: Algo[B]) extends Algo[B]

sealed abstract class Method[A,R] {
  val sym: IR.MtdSymbol
  val body: Variable[A] => Algo[R]
  override def hashCode() = sym.hashCode()
  override def equals(that: Any) = that match {
    case that: Method[_,_] => that.sym === sym
  }
  override def toString = s"${sym.asMethodSymbol.owner.name}.${sym.asMethodSymbol.name}"
}
case class LocalMethod[A,R](sym: IR.MtdSymbol, body: Variable[A] => Algo[R]) extends Method[A,R]
sealed abstract class NonLocalMethod[A,R] extends Method[A,R]
case class BlockingMethod[A,R](sym: IR.MtdSymbol, body: Variable[A] => Algo[R]) extends NonLocalMethod[A,R]
case class NonBlockingMethod[A](sym: IR.MtdSymbol, body: Variable[A] => Algo[Unit]) extends NonLocalMethod[A,Unit]

//case class State[A: CodeType](sym: IR.MtdSymbol, init: OpenCode[A])
case class State[A](sym: IR.MtdSymbol, init: OpenCode[A])(implicit val tpe: CodeType[A]) {
  override def toString = s"var ${sym.asMethodSymbol.owner.name}.${sym.asMethodSymbol.name}"
}

case class ActorType[A <: runtime.Actor](name: String, state: List[State[_]], methods: List[Method[_,_]], main: Algo[Unit], self: Variable[A])

case class Simulation(actorTypes: List[ActorType[_]], init: OpenCode[List[runtime.Actor]]) {
  
}
