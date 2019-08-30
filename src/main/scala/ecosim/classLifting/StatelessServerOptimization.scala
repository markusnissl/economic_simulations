package ecosim.classLifting

import ecosim.deep.member.{Actor, ActorType, LiftedMethod, State}
import ecosim.deep.IR
import IR.Predef._
import ecosim.deep.algo.{Algo, CallMethod, Foreach, Forever, IfThenElse, LetBinding, ScalaCode, Send}

//TODO check is it possible to have multiple instances of a stateless server with different states
// in that case it would be necessary to change the symbols of copied states and methods(change their name to "original symbol + original owner of symbol"
//TODO add 2 Waits when copying a stateless server
object StatelessServerOptimization {
  var actors: List[ActorType[_ <: Actor]] = _
  var modifiedStates: Map[IR.MtdSymbol, List[State[_]]] = Map()
  //The algorithm is iterative, this flag serves as a check whether there's been a change during the iteration
  var optimizationDone = false
  def optimizeActors(actors: List[ActorType[_ <: Actor]]) = {
    this.actors = actors
    while(!optimizationDone) {
      optimizationDone = true
      actors.foreach(actorType => {
        actorType.methods.foreach{case method: LiftedMethod[_] => {
          method.body = optimizeAlgo(method.body, actorType).asInstanceOf[Algo[method.R.Typ]]
        }}
        actorType.main = optimizeAlgo(actorType.main, actorType)
      })
    }
    actors
  }

  def findMethod(methodId: Int): LiftedMethod[_] = actors.flatMap(actor => actor.methods).filter(method => method.methodId == methodId).head

  def getFreeMethodId(): Int = actors.flatMap(actor => actor.methods).length

  //TODO refactor algo so that it has a trait composite which would force it to give its children, and recreate itself from the children. like map
  //if theres a change somewhere(only example for now is in Send where a message is sent to a stateless server),
  // the flag optimizationDone gets set to false
  def optimizeAlgo[A <: Actor,R: CodeType](algo: Algo[R], actorType: ActorType[A]): Algo[R] = {
    def substituteActorReferences[B: CodeType](algo: Algo[B], newActorReference: Variable[Actor], oldActorReference: Variable[Actor]): Algo[B] = {
      algo match {
        case lb: LetBinding[_, _] =>
          import lb.V
          val algoNew = LetBinding(lb.bound, substituteActorReferences(lb.value, newActorReference, oldActorReference), substituteActorReferences(lb.rest, newActorReference, oldActorReference))
          algoNew.asInstanceOf[Algo[B]]
        case foreach: Foreach[_, _] =>
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

    def copyCalledMethods[B: CodeType](algo: Algo[B], otherActorType: ActorType[_ <: Actor]): Algo[B] = {
      algo match {
        case lb: LetBinding[_, _] =>
          import lb.V
          val algoNew = LetBinding(lb.bound, copyCalledMethods(lb.value, otherActorType), copyCalledMethods(lb.rest, otherActorType))
          algoNew.asInstanceOf[Algo[B]]
        case foreach: Foreach[_, _] =>
          import foreach.{E, R}
          val algoNew = Foreach(foreach.ls, foreach.variable, copyCalledMethods(foreach.f, otherActorType))
          algoNew.asInstanceOf[Algo[B]]
        case forever: Forever[_] =>
          import forever.R
          val algoNew = Forever(copyCalledMethods(forever.body, otherActorType))
          algoNew.asInstanceOf[Algo[B]]
        case ite: IfThenElse[_] =>
          val algoNew = IfThenElse(ite.cond, copyCalledMethods(ite.ifBody, otherActorType), copyCalledMethods(ite.elseBody, otherActorType))
          algoNew.asInstanceOf[Algo[B]]
        case cm: CallMethod[_] =>
        //check if this method is already copied. if not - copy and call it, if it is just call it
          val methodToCopy = findMethod(cm.methodId)
          val methodId = copyMethod(methodToCopy, otherActorType)
          CallMethod(methodId, cm.argss)
        case other =>
          other.asInstanceOf[Algo[B]]
      }
    }

    def copyMethod(methodToCopy: LiftedMethod[_], otherActorType: ActorType[_ <: Actor]): Int = {
      val methodId = methodToCopy.methodId
      val foundMethod = actorType.methods.filter(myMethod => myMethod.sym == methodToCopy.sym)
      var callMethodId = -1
      //first check if this method has already been copied, if it has then just remember the id for call symbol
      if (foundMethod.nonEmpty) {
        callMethodId = foundMethod.head.methodId
        callMethodId
      }
      //else copy all of the states and this method into this actorType
      else {
        callMethodId = getFreeMethodId()
        val newMethodBody = copyCalledMethods(substituteActorReferences(methodToCopy.body, actorType.self.asInstanceOf[Variable[Actor]], otherActorType.self.asInstanceOf[Variable[Actor]]), otherActorType)
//        val newMethodBody = substituteActorReferences(methodToCopy.body, actorType.self.asInstanceOf[Variable[Actor]], otherActorType.self.asInstanceOf[Variable[Actor]])
        //TODO check why the type param of lifted method is Any
        val actorType1 = actorType
        actorType.methods = new LiftedMethod[Any](methodToCopy.cls, newMethodBody.asInstanceOf[Algo[Any]], methodToCopy.blocking, callMethodId) {
          override val mtd: cls.Method[Any, cls.Scp] = methodToCopy.mtd.asInstanceOf[this.cls.Method[Any, cls.Scp]]
          actorType = actorType1
        } :: actorType.methods
        //make sure not to copy some states twice
        val missingStates = methodToCopy.actorType.states.filterNot(state => actorType.states.exists(state2 => state2.sym == state.sym))
        missingStates.foreach(state => {
          //TODO change the state symbol to include the self variable of the object we copied it from. wont work
          actorType.states = state :: actorType.states
          modifiedStates = modifiedStates + (state.sym -> (actorType.states.head :: modifiedStates.getOrElse(state.sym, Nil)))
        })
        callMethodId
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
        //3. if it is, copy all of its states into this actorType (without copying the same states and methods multiple times)
        //3.1 , and this particular method, gotta change the references of the other object to this
        //4. generate a call method for the newly copied method
        //5. else, leave this send
        import send.R
        val method = findMethod(send.methodId)
        val otherActorType = method.actorType
        if (otherActorType.stateless) {
          optimizationDone = false //theres a change in this iteration, which means that the algorithm is not yet over
          //first check if this method has already been copied, if it has then just remember the id for call symbol
//          val foundMethod = actorType.methods.filter(myMethod => myMethod.sym == method.sym)
          var callMethodId = copyMethod(method, otherActorType)
//          if (foundMethod.nonEmpty) {
//            callMethodId = foundMethod.head.methodId
//          }
//          //else copy all of the states and this method into this actorType
//          else {
//            callMethodId = getFreeMethodId()
//            val newMethodBody = substituteActorReferences(method.body, actorType.self.asInstanceOf[Variable[Actor]], otherActorType.self.asInstanceOf[Variable[Actor]])
//            //TODO check why the type param of lifted method is Any
//            actorType.methods = new LiftedMethod[Any](method.cls, newMethodBody.asInstanceOf[Algo[Any]], method.blocking, callMethodId) {
//              override val mtd: cls.Method[Any, cls.Scp] = method.mtd.asInstanceOf[this.cls.Method[Any,cls.Scp]]
//            } :: actorType.methods
//            //make sure not to copy some states twice
//            val missingStates = method.actorType.states.filterNot(state => actorType.states.filter(state2 => state2.sym == state.sym).nonEmpty)
//            missingStates.foreach(state => {
//              actorType.states = state :: actorType.states
//              modifiedStates = modifiedStates + (state.sym -> (actorType.states.head :: modifiedStates.getOrElse(state.sym, Nil)))
//            })
//          }
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
