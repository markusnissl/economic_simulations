package ecosim.deep.member

import ecosim.deep.IR.Predef._
import ecosim.deep.algo.Algo

/**
  * Class representation of an actor containing the data needed for code generation
  *
  * @param name      of the actor, must be unique
  * @param states    init variables (at the moment must be all var)
  * @param methods   lifted methods of the class
  * @param main      main algorithm of the class (use main class)
  * @param self      (variable referencing to itself)
  * @param X         containg type of Actor
  * @param stateless is the actor type a stateless server, in order for it to be true, actors name has to end with 'stateless'
  * @tparam X        actor type
  */
case class ActorType[X <: Actor](name: String,
                                 var states: List[State[_]],
                                 methods: List[LiftedMethod[_]],
                                 main: Algo[_],
                                 self: Variable[X],
                                 stateless: Boolean = false)(implicit val X: CodeType[X]) {}

