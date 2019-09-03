package ecosim.classLifting

import ecosim.deep.IR
import IR.Predef._
import IR.TopLevel._
import IR.Predef.base.MethodApplication
import ecosim.deep.algo.{Algo, CallMethod, Foreach, Forever, IfThenElse, LetBinding, NoOp, ScalaCode, Send, Wait}
import ecosim.deep.member.{Actor, ActorType, LiftedMethod, RequestMessage, State}

/** Code lifter
  *
  * lifts the code into the deep representation [[ecosim.deep]]
  */
class Lifter {
  /** Maps method symbols to their IDs
    *
    */
  var methodsIdMap: Map[IR.MtdSymbol, Int] = Map()

  /** Maps method symbols to their methods' information, [[ecosim.classLifting.MethodInfo]]
    *
    */
  var methodsMap: Map[IR.MtdSymbol, MethodInfo[_]] = Map()

  //TODO fix waitTurns and lift if the actortype is a stateless server (naming convention, ending with stateless)
  /** Lifts the classes and object initialization
    *
    * @param startClasses - classes that need to be lifted, in form of [[Clasz]]
    * @param initializationClass - contains only one method, which has to return a list of [[Actor]]
    * @return deep embedding of the classes
    */
  def apply(startClasses: List[Clasz[_ <: Actor]], initializationClass: Clasz[_]): (List[ActorType[_]], OpenCode[List[Actor]]) = {
    var actorsInit: OpenCode[List[Actor]] = liftInitCode(initializationClass)
    //Collecting method symbols and info to generate methodsIdMap and methodsMap
    var counter = 0
    startClasses.map(c => c.methods).flatten
      .foreach(method => {
          import method.A
          methodsIdMap = methodsIdMap + (method.symbol -> counter)
          var blocking = true
          if (method.A <:< codeTypeOf[NBUnit]) blocking = false
          methodsMap = methodsMap + (method.symbol -> new MethodInfo[method.A](method.symbol, method.tparams, method.vparams, blocking))
          counter += 1
          simulation.Generator.getNextMethodId
      })
    //lifting types
    val endTypes = startClasses.map(c => {
      liftActor(c)
    })

    (endTypes, actorsInit)
  }

  /** Lifts a specific [[Actor]] class into an ActorType
    *
    * @param clasz representation of class which extends [[Actor]]
    * @tparam T - type of actor
    * @return an [[ActorType]] - deep embedding of an [[Actor]] class
    */
  private def liftActor[T <: Actor](clasz: Clasz[T]) = {
    import clasz.C
    val actorSelfVariable: Variable[_ <: Actor] = clasz.self.asInstanceOf[Variable[T]]
    //lifting states - class attributes
    var endStates: List[State[_]] = List()
    endStates = clasz.fields.map{case field => {
      import field.A
      State(field.symbol, field.A, field.init)
    }}
    var endMethods: List[LiftedMethod[_]] = List()
    var mainAlgo: Algo[_] = Forever(Wait())
    //lifting methods - with main method as special case
    clasz.methods.foreach(method => {
      val cde = method.body
      val mtdBody = liftCode(cde, actorSelfVariable, clasz)
      endMethods = (new LiftedMethod[Any](clasz, mtdBody, methodsMap(method.symbol).blocking, methodsIdMap(method.symbol)) {
        override val mtd: cls.Method[Any, cls.Scp] = method.asInstanceOf[this.cls.Method[Any,cls.Scp]]
      }) :: endMethods
      if (method.symbol.asMethodSymbol.name.toString() == "main") {
        mainAlgo = mtdBody
      }
    })
    ActorType[T](clasz.name, endStates, endMethods, mainAlgo, clasz.self.asInstanceOf[Variable[T]])
  }

  /** Lifts the code for actor initialization
    *
    * @param clasz - initialization class representation - must contain only 1 method, which returns an [[OpenCode]] of list of [[Actor]]s
    * @return - extracted initialization method body
    */
  private def liftInitCode(clasz: Clasz[_]): OpenCode[List[Actor]] = {
    //it's expected that this class' first method initializes actors
    val initMethod = clasz.methods.head
    val initCode = clasz.methods.head.body
    //check if the method returns a list of actors
    if (initMethod.A <:< codeTypeOf[List[Actor]])
      initCode.asInstanceOf[OpenCode[List[Actor]]]
    else {
      throw new Exception("The main method does not return a list of actors")
      code"List[Actor]()"
    }
  }

  /** Lifts an [[OpenCode]](expression) into its deep representation [[Algo]]
    *
    * @param cde - an [[OpenCode]] that will be lifted
    * @param actorSelfVariable - a self [[Variable]] of this actor, used to create messages
    * @param clasz - representatation of the [[Actor]] type, used to create a message handler for his methods
    * @tparam T - return type of the expression
    * @return [[Algo]] - deep representation of the expression
    */
  private def liftCode[T: CodeType](cde: OpenCode[T], actorSelfVariable: Variable[_ <: Actor], clasz: Clasz[_ <: Actor]): Algo[T] = {
    cde match {
      case code"val $x: $xt = $v; $rest: T" =>
        val f = LetBinding(Some(x), liftCode(v, actorSelfVariable, clasz), liftCode(rest, actorSelfVariable, clasz))
        f.asInstanceOf[Algo[T]]
      case code"$e; $rest: T" =>
        val f = LetBinding(None, liftCode(e, actorSelfVariable, clasz), liftCode(rest, actorSelfVariable, clasz))
        f.asInstanceOf[Algo[T]]
      case code"()" =>
        val f = NoOp()
        f.asInstanceOf[Algo[T]]
      case code"($x: List[$tb]).foreach[$ta](($y: tb) => $foreachbody)" =>
        val f: Foreach[tb.Typ, Unit] = Foreach(x, y, liftCode(code"$foreachbody; ()", actorSelfVariable, clasz))
        f.asInstanceOf[Algo[T]]
      case code"while(true) $body" =>
        val f = Forever(liftCode(body, actorSelfVariable, clasz))
        f.asInstanceOf[Algo[T]]
      case code"if($cond: Boolean) $ifBody:T else $elseBody: T" =>
        val f = IfThenElse(cond, liftCode(ifBody, actorSelfVariable, clasz), liftCode(elseBody, actorSelfVariable, clasz))
        f.asInstanceOf[Algo[T]]
      case code"SpecialInstructions.waitTurns()" =>
        val f = Wait()
        f.asInstanceOf[Algo[T]]
      case code"SpecialInstructions.handleMessages()" =>
        //generates an IfThenElse for each of this class' methods, which checks if the called method id is the same
        //as any of this class' methods, and calls the method if it is
        val resultMessageCall = Variable[Any]
        val p1 = Variable[RequestMessage]
        val algo: Algo[Any] = NoOp()
        val callCode = clasz.methods.foldRight(algo)((method, rest) => {
            val methodId = methodsIdMap(method.symbol)
            val methodInfo = methodsMap(method.symbol)
            //map method parameters correctly
            val argss: List[List[OpenCode[_]]] = methodInfo.vparams.zipWithIndex.map(x => {
              x._1.zipWithIndex.map(y => {
                code"$p1.argss(${Const(x._2)})(${Const(y._2)})"
              })
            })
            IfThenElse(code"$p1.methodId==${Const(methodId)}", CallMethod[Any](methodId, argss), rest)
        })

        //for each received message, use callCode
        val handleMessage = Foreach(
          code"$actorSelfVariable.popRequestMessages",
          p1, LetBinding(
            Option(resultMessageCall),
            callCode,
            ScalaCode(code"""$p1.reply($actorSelfVariable, $resultMessageCall)""")
          )
        )
        handleMessage.asInstanceOf[Algo[T]]
      case code"${MethodApplication(ma)}:Any" if methodsIdMap.get(ma.symbol).isDefined =>
        //extracting arguments and formatting them
        val argss = ma.args.tail.map(args => args.toList.map(arg => code"$arg")).toList
        //method is local - method recipient is this(self)
        val recipientActorVariable = ma.args.head.head.asInstanceOf[OpenCode[Actor]]
        if(actorSelfVariable == recipientActorVariable) {
          val f = CallMethod(methodsIdMap(ma.symbol), argss)
          f.asInstanceOf[Algo[T]]
        }
        //method recipient is another actor - a message has to be sent
        else {
          val f = Send(actorSelfVariable.toCode, recipientActorVariable, methodsIdMap(ma.symbol), argss, methodsMap(ma.symbol).blocking)
          f.asInstanceOf[Algo[T]]
        }
      case _ =>
        //here there is space for some more code patterns to be lifted, by using the liftCodeOther method which can be overriden
        val liftedCode = liftCodeOther(cde, actorSelfVariable, clasz)
        //if liftCodeOther returns something, return that
        if (liftedCode.isDefined) {
          liftedCode.get
        }
        //otherwise, analyze if the cde is legitimate ScalaCode (does not contain any other recognizable code pattern
        // somewhere inside (e.g. an unsupported code pattern could contain a Foreach somewhere inside of it and that
        // would cause problems if it was lifted as ScalaCode)
        else {
          cde analyse {
            case d if d != cde =>
              val c = liftCode(d, actorSelfVariable, clasz)
              c match {
                case scalacode: ScalaCode[_] =>
                case _ => throw new Exception("Unsupported code inside " + cde)
              }
          }
          val f = ScalaCode(cde)
          f.asInstanceOf[Algo[T]]
        }
    }
  }

  /** Used for operations that were not covered in [[liftCode]]. Lifts an [[OpenCode]](expression) into its deep representation [[Algo]]
    *
    * @param cde - an [[OpenCode]] that will be lifted
    * @param actorSelfVariable - a self [[Variable]] of this actor, used to create messages
    * @param clasz - representatation of the [[Actor]] type, used to create a message handler for his methods
    * @tparam T - return type of the expression
    * @return [[Algo]] - deep representation of the expression
    */
  def liftCodeOther[T: CodeType](cde: OpenCode[T], actorSelfVariable: Variable[_ <: Actor], clasz: Clasz[_ <: Actor]): Option[Algo[T]] = {
    None
  }
}

