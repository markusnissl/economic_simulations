package ecosim.classLifting

import ecosim.deep.member.{Actor, ActorType, LiftedMethod}
import ecosim.deep.IR
import IR.Predef._
import ecosim.deep.algo.{Algo, CallMethod, Foreach, Forever, IfThenElse, LetBinding, ScalaCode, Send}

object StatelessServerOptimization {
  var actors: List[ActorType[_ <: Actor]] = _
  def optimizeActors(actors: List[ActorType[_ <: Actor]]) = {
    this.actors = actors
    //TODO: do this iteratively until theres no more changes
    actors.foreach(actorType => {
      actorType.methods.foreach{case method: LiftedMethod[_] => {
      //TODO: change body, not body2. remove body2
        method.body = optimizeAlgo(method.body, actorType).asInstanceOf[Algo[method.R.Typ]]
      }}
      actorType.main = optimizeAlgo(actorType.main, actorType)
    })
    actors
  }

  def findMethod(methodId: Int): LiftedMethod[_] = actors.flatMap(actor => actor.methods).filter(method => method.methodId == methodId).head

  def getFreeMethodId(): Int = actors.flatMap(actor => actor.methods).length

  //TODO refactor algo so that it has a trait composite which would force it to give its children, and recreate itself from the children. like map
  def optimizeAlgo[A <: Actor,R: CodeType](algo: Algo[R], actorType: ActorType[A]): Algo[R] = {
    def substituteActorReferences[B: CodeType](algo: Algo[B], newActorReference: Variable[Actor], oldActorReference: Variable[ Actor]): Algo[B] = {
      algo match {
        case lb: LetBinding[_,_] =>
          import lb.V
          val algoNew = LetBinding(lb.bound, substituteActorReferences(lb.value, newActorReference, oldActorReference), substituteActorReferences(lb.rest, newActorReference, oldActorReference))
          algoNew.asInstanceOf[Algo[B]]
        case foreach: Foreach[_,_] =>
          import foreach.{E, R}
          val algoNew = Foreach(foreach.ls, foreach.variable, substituteActorReferences(foreach.f, newActorReference, oldActorReference))
          algoNew.asInstanceOf[Algo[B]]
        case forever: Forever[_] =>
          import forever.R
          val algoNew = Forever(substituteActorReferences(forever.body, newActorReference, oldActorReference))
          algoNew.asInstanceOf[Algo[B]]
        case ite: IfThenElse[_] =>
          val algoNew = IfThenElse(ite.cond, substituteActorReferences(ite.ifBody, newActorReference, oldActorReference), substituteActorReferences(ite.elseBody, newActorReference, oldActorReference))
          algoNew.asInstanceOf[Algo[B]]
        case send: Send[_] =>
          // change the reference to this
          val algoNew = Send(newActorReference.toCode, send.actorRef, send.methodId, send.argss, send.blocking)(codeTypeOf[B])
          algoNew.asInstanceOf[Algo[B]]
        case ScalaCode(cde) =>
          //substitute all the old references to this
          val newCde = oldActorReference.substitute(cde.unsafe_asClosedCode, newActorReference.toCode)
          val f = ScalaCode(newCde)
          f.asInstanceOf[Algo[B]]
        case other =>
          other.asInstanceOf[Algo[B]]
      }
    }

    algo match {
      case lb: LetBinding[_,_] =>
        import lb.{V}
        val algoNew = LetBinding(lb.bound, optimizeAlgo(lb.value, actorType), optimizeAlgo(lb.rest, actorType))
        algoNew.asInstanceOf[Algo[R]]
      case foreach: Foreach[_,_] =>
        import foreach.{E, R}
        val algoNew = Foreach(foreach.ls, foreach.variable, optimizeAlgo(foreach.f, actorType))
        algoNew.asInstanceOf[Algo[R]]
      case forever: Forever[_] =>
        import forever.R
        val algoNew = Forever(optimizeAlgo(forever.body, actorType))
        algoNew.asInstanceOf[Algo[R]]
      case ite: IfThenElse[_] =>
//        import ite.A
        val algoNew = IfThenElse(ite.cond, optimizeAlgo(ite.ifBody, actorType), optimizeAlgo(ite.elseBody, actorType))
        algoNew.asInstanceOf[Algo[R]]
      case send: Send[_] =>
        //this is the case we are optimizing
        //1. find the owner actortype of this method
        //2. check if its stateless
        //3. if it is, copy all of its states into this actorType
        //3.1 , and this particular method, gotta change the references of the other object to this
        //4. generate a call method for the newly copied method
        //5. else, leave this send
        import send.R
        val method = findMethod(send.methodId)
        val otherActorType = method.actorType
        if (otherActorType.stateless) {
          //first check if this method has already been copied, if it has then just remember the id for call symbol
          val foundMethod = actorType.methods.filter(myMethod => myMethod.sym == method.sym)
          var callMethodId = -1
          if (foundMethod.nonEmpty) {
            callMethodId = foundMethod.head.methodId
          }
          //else copy all of the states and this method into this actorType
          else {
            callMethodId = getFreeMethodId()
            val newMethodBody = substituteActorReferences(method.body, actorType.self.asInstanceOf[Variable[Actor]], otherActorType.self.asInstanceOf[Variable[Actor]])
            actorType.methods = new LiftedMethod[Any](method.cls, newMethodBody.asInstanceOf[Algo[Any]], method.blocking, callMethodId) {
              override val mtd: cls.Method[Any, cls.Scp] = method.mtd.asInstanceOf[this.cls.Method[Any,cls.Scp]]
            } :: actorType.methods
            //make sure not to copy some states twice
            val missingStates = method.actorType.states.filterNot(state => actorType.states.filter(state2 => state2.sym == state.sym).nonEmpty)
            actorType.states = actorType.states ::: missingStates
          }
          val newAlgo = CallMethod[method.R.Typ](callMethodId, send.argss)
          newAlgo.asInstanceOf[Algo[R]]
        } else {
          algo.asInstanceOf[Algo[R]]
        }
      case other =>
        other.asInstanceOf[Algo[R]]
    }
  }
}
