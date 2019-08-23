package ecosim.deep.member

import ecosim.deep.algo.Algo
import ecosim.deep.IR.Predef._

/**
  * Class representation of an actor containing the data needed for code generation
  * @param name of the actor, must be unique
  * @param states init variables (at the moment must be all var)
  * @param methods lifted methods of the class
  * @param main main algorithm of the class (use main class)
  * @param self (variable referencing to itself)
  * @param X containg type of Actor
  * @tparam X actor type
  */
case class ActorType[X <: Actor](name: String,
                                 states: List[State[_]],
                                 methods: List[LiftedMethod[_]],
                                 main: Algo[_],
                                 self: Variable[X])(implicit val X: CodeType[X]) {}

