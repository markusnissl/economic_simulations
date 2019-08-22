package ecosim.classLifting

import ecosim.deep._
import ecosim.deep.IR
import IR.Predef._
import IR.TopLevel._
import IR.Predef.base.MethodApplication
import ecosim.runtime.Actor
import squid.ir.RuntimeSymbols.MtdSymbol

/** Code lifter
  *
  * lifts the code into the deep representation [[ecosim.deep]]
  */
class Lifter {
  /** Maps method symbols to their IDs
    *
    */
  var methodsIdMap: Map[IR.MtdSymbol, Int] = Map()

  //TODO make it MtdSymbol - methodInfo
  /** Maps method IDs to their methods' information, [[ecosim.classLifting.MethodInfo]]
    *
    */
  var methodsMap: Map[Int, MethodInfo[_]] = Map()


  /** Lifts the classes and object initialization
    *
    * @param startClasses - classes that need to be lifted, in form of [[Clasz]]
    * @param initializationClass - contains only one method, which has to return a list of [[ecosim.runtime.Actor]]
    * @return deep embedding of the classes
    */
  def apply(startClasses: List[Clasz[_ <: Actor]], initializationClass: Clasz[_]): ecosim.deep.Simulation = {
    var actorsInit: OpenCode[List[Actor]] = liftInitCode(initializationClass)
    //Collecting method symbols to generate methodsIdMap
    var counter = 0
    startClasses.map(c => c.methods).flatten
      .foreach(method => {
          import method.A
          import method.Scp
          methodsIdMap = methodsIdMap + (method.symbol -> counter)
          var blocking = true
          if (method.A <:< codeTypeOf[NBUnit]) blocking = false
          methodsMap = methodsMap + (counter -> new MethodInfo[method.A](method.symbol, method.tparams, method.vparams, blocking))
          counter += 1
      })

    val endTypes = startClasses.map(c => {
      liftActor(c)
    })
    ecosim.deep.Simulation(endTypes, actorsInit, methodsIdMap, methodsMap)
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
    var endStates: List[State[_]] = List()
    endStates = clasz.fields.map{case field => {
      import field.A
      State(field.symbol, field.init)
    }}
    var endMethods: List[LiftedMethod[_]] = List()
    var mainAlgo: Algo[_] = Forever(Wait(code"1"))
    clasz.methods.foreach(method => {
      val cde = method.body
      val mtdBody = liftCode(cde, actorSelfVariable, clasz)
      endMethods = (new LiftedMethod[Any](clasz, mtdBody, methodsMap(methodsIdMap(method.symbol)).blocking) {
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
    val initMethod = clasz.methods.head
    val initCode = clasz.methods.head.body
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
  private def liftCode[T: CodeType](cde: OpenCode[T], actorSelfVariable: Variable[_ <: Actor], clasz: Clasz[_ <: ecosim.runtime.Actor]): Algo[T] = {
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
        val f = IfElse(cond, liftCode(ifBody, actorSelfVariable, clasz), liftCode(elseBody, actorSelfVariable, clasz))
        f.asInstanceOf[Algo[T]]
      case code"SpecialInstructions.waitTurns($x)" =>
        val f = Wait(x)
        f.asInstanceOf[Algo[T]]
      case code"SpecialInstructions.handleMessages()" =>
        val resultMessageCall = Variable[Any]
        val p1 = Variable[_root_.simulation.RequestMessageInter]
        val algo: Algo[Any] = NoOp()
        val callCode = clasz.methods.foldRight(algo)((method, rest) => {
            val methodId = methodsIdMap(method.symbol)
            val methodInfo = methodsMap(methodId)
            val argss: List[List[OpenCode[_]]] = methodInfo.vparams.zipWithIndex.map(x => {
              x._1.zipWithIndex.map(y => {
                code"$p1.argss(${Const(x._2)})(${Const(y._2)})"
              })
            })
            IfElse(code"$p1.methodId==${Const(methodId)}", CallMethod[Any](methodId, argss), rest)
        })
        val handleMessage = Foreach(
          code"$actorSelfVariable.getRequestMessages",
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
        //method is local
        val recipientActorVariable = ma.args.head.head.asInstanceOf[OpenCode[Actor]]
        if(actorSelfVariable == recipientActorVariable) {
          val f = CallMethod(methodsIdMap(ma.symbol), argss)
          f.asInstanceOf[Algo[T]]
        }
        //method recipient is another actor
        else {
          val f = Send(actorSelfVariable.toCode, recipientActorVariable, Message(methodsIdMap(ma.symbol), argss))
          f.asInstanceOf[Algo[T]]
        }
      case _ =>
        val liftedCode = liftCodeOther(cde, actorSelfVariable, clasz)
        if (liftedCode.isDefined) {
          liftedCode.get
        }
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
  def liftCodeOther[T: CodeType](cde: OpenCode[T], actorSelfVariable: Variable[_ <: Actor], clasz: Clasz[_ <: ecosim.runtime.Actor]): Option[Algo[T]] = {
    None
  }
}

object App1 extends App {
  val cls1: ClassWithObject[Actor1] = Actor1.reflect(IR)
  val cls2: ClassWithObject[Actor2] = Actor2.reflect(IR)
  val cls3: ClassWithObject[MainClass] = MainClass.reflect(IR)
  val startClasses: List[Clasz[_ <: Actor]] = List(cls1, cls2)
  val mainClass = cls3
  val lifter = new Lifter()
  val simulation1 = lifter(startClasses, mainClass)
  val actors1 = simulation1.codegen()
  val simu1 = new old.Simulation()
  simu1.init(actors1)
  simu1.run(10)
}
