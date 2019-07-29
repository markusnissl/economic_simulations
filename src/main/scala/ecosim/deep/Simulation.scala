package ecosim
package deep

import squid.IR
import squid.IR.Predef._

case class Message[A,R](sym: IR.MtdSymbol, arg: OpenCode[A])

sealed abstract class Algo[R]
case class Forever(body: Algo[Unit]) extends Algo[Unit]
case class Send[R](actorRef: OpenCode[runtime.Actor], msg: Message[_,R]) extends Algo[Unit]
case class Foreach[A](ls: OpenCode[List[A]], f: Variable[A] => Algo[Unit]) extends Algo[Unit]
case class ScalaCode[A](cde: OpenCode[A]) extends Algo[A]

sealed abstract class Method[A,R] {
  val sym: IR.MtdSymbol
  val body: Algo[R]
}
case class BlockingMethod[A,R](sym: IR.MtdSymbol, body: Algo[R]) extends Method[A,R]
case class NonBlockingMethod[A](sym: IR.MtdSymbol, body: Algo[Unit]) extends Method[A,Unit]
case class LocalMethod[A,R](sym: IR.MtdSymbol, body: Algo[R]) extends Method[A,R]

//case class State[A: CodeType](sym: IR.MtdSymbol, init: OpenCode[A])
case class State[A](sym: IR.MtdSymbol, init: OpenCode[A])(implicit val tpe: CodeType[A])

case class ActorType[A <: runtime.Actor](name: String, state: List[State[_]], methods: List[Method[_,_]], self: Variable[A])

case class Simulation(actorTypes: List[ActorType[_]], init: OpenCode[List[runtime.Actor]]) {
  
}
