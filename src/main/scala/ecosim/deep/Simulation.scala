package ecosim
package deep

import IR.Predef._

case class Message[A,R](mtd: NonLocalMethod[A,R], arg: OpenCode[A])

sealed abstract class Algo[A](implicit val tpe: CodeType[A])
case class Forever(body: Algo[_]*) extends Algo[Unit]
case class Block(body: Algo[_]*) extends Algo[Unit]
case class Wait(cde: OpenCode[Int]) extends Algo[Unit]
case class CallMethod[E, R: CodeType](mtd: Method[E,R], arg: OpenCode[E])(implicit val E: CodeType[E]) extends Algo[R]
case class CallMethodC[E, R: CodeType](mtd: OpenCode[Method[E,R]], arg: OpenCode[E])(implicit val E: CodeType[E]) extends Algo[R]
case class Send[E,R:CodeType](actorRef: OpenCode[runtime.Actor], msg: Message[E,R])(implicit val E: CodeType[E]) extends Algo[R]
case class Foreach[E, R: CodeType](ls: OpenCode[List[E]], f: Variable[E] => Algo[R])(implicit val E: CodeType[E]) extends Algo[Unit]
case class ScalaCode[A: CodeType](cde: OpenCode[A]) extends Algo[A]
case class ScalaCodeWrapper[A: CodeType](cde: OpenCode[A]) extends Algo[A]
case class LetBinding[V: CodeType, A: CodeType](bound: Variable[V], value: OpenCode[V], body: Algo[A]) extends Algo[A]
case class LetBinding2[V: CodeType, A: CodeType](bound: Variable[V], value: Algo[V], body: Algo[A])(implicit val V: CodeType[V]) extends Algo[A]

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
