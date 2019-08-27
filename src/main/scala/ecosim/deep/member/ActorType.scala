package ecosim.deep.member

import ecosim.deep.algo.Algo
import ecosim.deep.IR.Predef._

/**
  * Class representation of an actorType containing the data needed for code generation
  * @param name of the actorType, must be unique
  * @param states init variables (at the moment must be all var)
  * @param methods lifted methods of the class
  * @param main main algorithm of the class (use main class)
  * @param self (variable referencing to itself)
  * @param X containg type of Actor
  * @tparam X actorType type
  */
case class ActorType[X <: Actor](name: String,
                                 var states: List[State[_]],
                                 var methods: List[LiftedMethod[_]],
                                 var main: Algo[_],
                                 self: Variable[X],
                                 var stateless: Boolean = true)(implicit val X: CodeType[X]) {}

