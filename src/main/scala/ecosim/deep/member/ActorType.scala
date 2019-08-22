package ecosim.deep.member

import ecosim.deep.algo.Algo
import ecosim.deep.IR.Predef._
import simulation.core.Actor

case class ActorType[X <: Actor](name: String, states: List[State[_]], methods: List[LiftedMethod[_]], main: Algo[_], self: Variable[X])(implicit val X: CodeType[X]) {

}

