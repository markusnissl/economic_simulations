package ecosim.classLifting

import ecosim.deep._
import ecosim.deep.IR
import IR.Predef._
import IR.TopLevel._
import IR.Predef.base.MethodApplication
import ecosim.runtime.Actor
import squid.ir.RuntimeSymbols.MtdSymbol

class Lifter {
  var methodsIdMapReversed: Map[IR.MtdSymbol, Int] = Map()
  var actorSelfVariable: Variable[_ <: Actor] = null
  var methodsIdMap: Map[Int, IR.MtdSymbol] = Map()

  def apply(startClasses: List[Clasz[_ <: Actor]], mainClass: Clasz[_]): ecosim.deep.Simulation = {
    var actorsInit: OpenCode[List[Actor]] = liftInitCode(mainClass)
    //Collecting method symbols to generate methodsIdMap
    var counter = 0
    startClasses.map(c => c.methods.map(m => m.symbol)).flatten
      .foreach(methodSym => {
        methodsIdMap = methodsIdMap + (counter -> methodSym)
        methodsIdMapReversed = methodsIdMapReversed + (methodSym -> counter)
        counter += 1
      })

    val endTypes = startClasses.map(c => {
      liftActor(c)
    })
    ecosim.deep.Simulation(endTypes, actorsInit, methodsIdMap)
  }

  def liftActor[T <: ecosim.runtime.Actor](clasz: Clasz[T]) = {
    import clasz.C
    actorSelfVariable = clasz.self.asInstanceOf[Variable[T]]
    var endStates: List[State[_]] = List()
    //TODO check if this works well, seems like theres something wrong with the type of created states
    endStates = clasz.fields.map(field => {
      //DOESNT WORK, might need to give its type
      import field.A
      State(field.symbol, field.init)
    })
    var endMethods: List[Method[_, _]] = List()
    var mainAlgo: Algo[Unit] = Forever(Wait(code"1"))
    clasz.methods.map(method => {
      val cde = method.body
      //TODO check if theres no input parameters
      if (method.symbol.asMethodSymbol.name.toString() == "main") {
        val algo = liftCode(cde)
        if (algo.tpe <:< codeTypeOf[Unit]){
          mainAlgo = liftCode(cde).asInstanceOf[Algo[Unit]]
        } else {
          //TODO: else, throw an exception? or make it so that main in deep.Simulation can be of type Algo[Any]
          println("Warning! Main method of class " + clasz.name + " has a return value")
        }

      } else {
        val mtdBody = liftCode(cde)
        val params = method.vparams.flatten
        //TODO fix the input parameters of the method body
        endMethods = LocalMethod(IR.MtdSymbol(method.symbol), (par1: Variable[Int]) => mtdBody ):: endMethods
      }
    })
    ActorType[T](clasz.name, endStates, endMethods, mainAlgo, clasz.self.asInstanceOf[Variable[T]])
  }

  def liftInitCode(clasz: Clasz[_]): OpenCode[List[Actor]] = {
    val initMethod = clasz.methods.head
    val initCode = clasz.methods.head.body
    if (initMethod.A <:< codeTypeOf[List[Actor]])
      initCode.asInstanceOf[OpenCode[List[Actor]]]
    //TODO else throw an exception?
    else {
      println("Warning! The main method does not return a list of actors. The Simulation will start without actors")
      code"List[Actor]()"
    }
  }

  def liftCode[T: CodeType](cde: OpenCode[T]): Algo[T] = {
//    base.debugFor(
    cde match {
      case code"val $x: $xt = $v; $rest: T" =>
        LetBinding(Some(x), liftCode(v), liftCode(rest))
      case code"$e; $rest: T" =>
        LetBinding(None, liftCode(e), liftCode(rest))
      case code"($x: List[$tb]).foreach[$ta](($y: tb) => $foreachbody)" =>
        val f: Foreach[tb.Typ, Unit] = Foreach(x, y, liftCode(code"$foreachbody; ()"))
        f.asInstanceOf[Algo[T]]
      case code"while(true) $body" =>
        val f = Forever(liftCode(body))
        f.asInstanceOf[Algo[T]]
      case code"SpecialOperations.waitTurns($x)" =>
        val f = Wait(x)
        f.asInstanceOf[Algo[T]]
//      case code"${MethodApplication(ma)}:Any" if ma.symbol.asMethodSymbol.name.toString() == "waitTurns" =>
//        val f = Wait(code"1")
//        f.asInstanceOf[Algo[T]]
      case code"${MethodApplication(ma)}:Any" if methodsIdMapReversed.get(ma.symbol).isDefined =>
        //method is local
        if(actorSelfVariable == ma.args(0)(0)) {
          //TODO generate CallMethodC
          //CallMethodC(code"$methodsIdMap[ma.symbol]", code"bro")
          null
        }
        //method includes another agent
        else {
          //TODO generate Send
          //Send(null, null, null)
          null
        }
      case _ =>
        //TODO check if inside a scala code, there is one of the supported operations(e.g. if the body of 'if' contains a foreach)
        cde analyse {
          case d => println(d)
        }
        ScalaCode(cde)
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
  val simu1 = new _root_.Simulation.Simulation()
  simu1.init(actors1)
  simu1.run(10)

}
