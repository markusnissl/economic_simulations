package ecosim.classLifting

import ecosim.deep._
import ecosim.deep.IR
import IR.Predef._
import IR.TopLevel._
import IR.Predef.base.MethodApplication
import ecosim.runtime.Actor
import squid.ir.RuntimeSymbols.MtdSymbol

class Lifter {
  def liftActor[T <: ecosim.runtime.Actor](clasz: Clasz[T])= {
    import clasz.C

      var endStates: List[State[_]] = List()
      //TODO check if this works well, seems like theres something wrong with the type of created states
      clasz.fields.foreach(field => {
        import field.A
        //TODO ask if this is a correct way to instantiate a state symbol, also goes for method symbols later in the code
        //endStates = State[A](IR.MtdSymbol(field.symbol), field.init) :: endStates
      })
      var endMethods: List[LiftedMethod[_]] = List()
      var mainAlgo: Algo[Unit] = Forever(Wait(code"1"))
      clasz.methods.foreach(method => {
        val cde = method.body
        //TODO: give a better way to label the main loop method
        if (method.symbol.asMethodSymbol.name.toString() == "loop") {
          val algo = liftCode(cde)
          //TODO: fix this, the check doesn't work
          if (algo.isInstanceOf[Algo[Unit]])
            mainAlgo = liftCode(cde).asInstanceOf[Algo[Unit]]
          //TODO: else, throw an exception? or make it so that main in deep.Simulation can be of type Algo[Any]
        } else {
          val mtdBody = liftCode(cde)
          val params = method.vparams.flatten
          //TODO fix the input parameters of the method body
          endMethods = (new LiftedMethod[Any](clasz, mtdBody, true) {
            override val mtd: cls.Method[Any, cls.Scp] = method.asInstanceOf[this.cls.Method[Any,cls.Scp]]
          }) :: endMethods
        }
      })
    (ActorType[T](clasz.name, endStates, endMethods, mainAlgo, clasz.self.asInstanceOf[Variable[T]]), endMethods)
  }

  def liftInitCode(clasz: Clasz[_]): OpenCode[List[Actor]] = {
    val initCode = clasz.methods.head.body
    //TODO fix this, the check doesn't work
    if (initCode.isInstanceOf[OpenCode[List[Actor]]])
      initCode.asInstanceOf[OpenCode[List[Actor]]]
    //TODO else throw an exception?
    else {
      code"List[Actor]()"
    }
  }
  def apply(startClasses: List[Clasz[_ <: Actor]], mainClass: Clasz[_]): ecosim.deep.Simulation = {
    var actorsInit: OpenCode[List[Actor]] = liftInitCode(mainClass)
    var methodsIdMap: Map[ecosim.deep.IR.MtdSymbol, Int] = Map()
    var counter = 0
    val endTypesMethods = startClasses.map(c => {
      liftActor(c)
    })
    var endTypes = endTypesMethods.foldRight(List[ActorType[_]]())((a, endTypes) => a._1 :: endTypes)
    endTypesMethods.foreach(typeMethods => {
      typeMethods._2.foreach(method => {
        methodsIdMap = methodsIdMap + (method.sym -> counter)
        counter += 1
      })
    })
    ecosim.deep.Simulation(endTypes, actorsInit, methodsIdMap)
  }

  def liftCode[T: CodeType](cde: OpenCode[T]): Algo[T] = {
//    base.debugFor(
    cde match {
      case code"val $x: $xt = $v; $rest: T" =>
        LetBinding(Some(x), ScalaCode(v), liftCode(rest))
      case code"$e; $rest: T" =>
        LetBinding(None, liftCode(e), liftCode(rest))
      case code"($x: List[$tb]).foreach[$ta](($y: tb) => $foreachbody)" =>
        val f: Foreach[tb.Typ, Unit] = Foreach(x, y, liftCode(code"$foreachbody; ()"))
        f.asInstanceOf[Algo[T]]
      case code"while(true) $body" =>
        val f = Forever(liftCode(body))
        f.asInstanceOf[Algo[T]]
      case code"${MethodApplication(ma)}:Any" if ma.symbol.asMethodSymbol.name.toString() == "waitTurns" =>
        val f = Wait(code"1")
        f.asInstanceOf[Algo[T]]
      //TODO method calls, and message sending
      //TODO check if inside a scala code, there is one of the supported operations(e.g. if the body of 'if' contains a foreach)
//      case code"${MethodApplication(ma)}:Any" => println(ma.args); null
//      case code"($x: Actor1).$met($y: Int)" =>
//        println(x)
//        null
//        println(obj)
//        println(meth)
//        println(params)
//        ScalaCode(cde)
      case _ => ScalaCode(cde)
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
  //TODO: ask how to know which subtype of Clasz to use, and how to find all of the annotaded classes
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
