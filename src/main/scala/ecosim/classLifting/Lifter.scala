package ecosim.classLifting

import ecosim.deep._
import ecosim.deep.IR
import IR.Predef._
import IR.TopLevel._
import IR.Predef.base.MethodApplication
import ecosim.runtime.Actor
import squid.ir.RuntimeSymbols.MtdSymbol

class Lifter {
  var methodsMap: Map[Int, MethodInfo[_]] = Map()
  var methodsIdMap: Map[IR.MtdSymbol, Int] = Map()

  def apply(startClasses: List[Clasz[_ <: Actor]], mainClass: Clasz[_]): ecosim.deep.Simulation = {
    var actorsInit: OpenCode[List[Actor]] = liftInitCode(mainClass)
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

  def liftActor[T <: ecosim.runtime.Actor](clasz: Clasz[T]) = {
    import clasz.C
    val actorSelfVariable: Variable[_ <: Actor] = clasz.self.asInstanceOf[Variable[T]]
    var endStates: List[State[_]] = List()
    //TODO check if this works well, seems like theres something wrong with the type of created states
    endStates = clasz.fields.map(field => {
      //DOESNT WORK, might need to give its type
      import field.A
      State(field.symbol, field.init)
    })
    var endMethods: List[LiftedMethod[_]] = List()
    var mainAlgo: Algo[_] = Forever(Wait(code"1"))
    clasz.methods.foreach(method => {
      val cde = method.body
      //TODO check if theres no input parameters
      if (method.symbol.asMethodSymbol.name.toString() == "main") {
        mainAlgo = liftCode(cde, actorSelfVariable)
      } else {
        val mtdBody = liftCode(cde, actorSelfVariable)
        endMethods = (new LiftedMethod[Any](clasz, mtdBody, methodsMap(methodsIdMap(method.symbol)).blocking) {
          override val mtd: cls.Method[Any, cls.Scp] = method.asInstanceOf[this.cls.Method[Any,cls.Scp]]
        }) :: endMethods
      }
    })
    ActorType[T](clasz.name, endStates, endMethods, mainAlgo, clasz.self.asInstanceOf[Variable[T]])
  }

  def liftInitCode(clasz: Clasz[_]): OpenCode[List[Actor]] = {
    val initMethod = clasz.methods.head
    val initCode = clasz.methods.head.body
    if (initMethod.A <:< codeTypeOf[List[Actor]])
      initCode.asInstanceOf[OpenCode[List[Actor]]]
    else {
      throw new Exception("The main method does not return a list of actors")
      code"List[Actor]()"
    }
  }

  def liftCode[T: CodeType](cde: OpenCode[T], actorSelfVariable: Variable[_ <: Actor]): Algo[T] = {
    cde match {
      case code"val $x: $xt = $v; $rest: T" =>
        val f = LetBinding(Some(x), liftCode(v, actorSelfVariable), liftCode(rest, actorSelfVariable))
        f.asInstanceOf[Algo[T]]
      case code"$e; $rest: T" =>
        val f = LetBinding(None, liftCode(e, actorSelfVariable), liftCode(rest, actorSelfVariable))
        f.asInstanceOf[Algo[T]]
      case code"($x: List[$tb]).foreach[$ta](($y: tb) => $foreachbody)" =>
        val f: Foreach[tb.Typ, Unit] = Foreach(x, y, liftCode(code"$foreachbody; ()", actorSelfVariable))
        f.asInstanceOf[Algo[T]]
      case code"while(true) $body" =>
        val f = Forever(liftCode(body, actorSelfVariable))
        f.asInstanceOf[Algo[T]]
      case code"if($cond: Boolean) $ifBody:T else $elseBody: T" if elseBody != code"()" =>
        val f = IfElse(cond, liftCode(ifBody, actorSelfVariable), liftCode(elseBody, actorSelfVariable))
        f.asInstanceOf[Algo[T]]
      case code"if($cond: Boolean) $body:T" =>
        val f = If(cond, liftCode(body, actorSelfVariable))
        f.asInstanceOf[Algo[T]]
      case code"SpecialInstructions.waitTurns($x)" =>
        val f = Wait(x)
        f.asInstanceOf[Algo[T]]
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
        //TODO write a better exception message
        cde analyse {
          case d if d != cde =>
            val c = liftCode(d, actorSelfVariable)
            c match {
              case scalacode: ScalaCode[_] =>
              case _ => throw new Exception("Unsupported code inside " + cde)
            }
        }
        val f = ScalaCode(cde)
        f.asInstanceOf[Algo[T]]
    }
//    )
  }
}

//      val f: Foreach[Any, ta.Typ] = Foreach(x, (z: Variable[Any]) => {
//        val s_body = y.substitute(foreachbody.unsafe_asClosedCode, z.toCode)
//        liftCode(s_body)
//      })
//      f.asInstanceOf[Algo[T]]

object App1 extends App {
//  val a = code"List(1,2,3).foreach(x => println(x))"
//  a.analyse {
//    case c => println(c)
//  }

  val cls1: ClassWithObject[Actor1] = Actor1.reflect(IR)
  val cls2: ClassWithObject[Actor2] = Actor2.reflect(IR)
  val cls3: ClassWithObject[MainClass] = MainClass.reflect(IR)
  val startClasses: List[Clasz[_ <: Actor]] = List(cls1, cls2)
  val mainClass = cls3
  val lifter = new Lifter()
  val simulation1 = lifter(startClasses, mainClass)
  val actors1 = simulation1.codegen()
  /*val simu1 = new old.Simulation()
  simu1.init(actors1)
  simu1.run(10)*/

}
