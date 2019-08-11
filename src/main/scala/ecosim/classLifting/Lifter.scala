package ecosim.classLifting

import ecosim.deep._
import ecosim.deep.IR
import IR.Predef._
import IR.TopLevel._
import IR.Predef.base.MethodApplication
import ecosim.runtime.Actor

class Lifter {
  def apply(startClasses: List[Clasz[_]]) = {
    var endTypes: List[ActorType[_]] = List()
    var actorsInit: OpenCode[List[Actor]] = code"List()"
    startClasses.foreach((clasz: Clasz[_]) => {
      if (clasz.name == "MainClass") { //actors initialization code
        val initCode = clasz.methods.head.body
        //TODO fix this, the check doesn't work
        if (initCode.isInstanceOf[OpenCode[List[Actor]]])
          actorsInit = initCode.asInstanceOf[OpenCode[List[Actor]]]
      }
        //TODO: else, throw an exception?
      else{  //ActorType initialization
        var endStates: List[State[_]] = List()
        //TODO check if this works well, seems like theres something wrong with the type of created states
        clasz.fields.foreach(field => {
          endStates = State(IR.MtdSymbol(field.symbol), field.init) :: endStates
          //endStates = State(IR.methodSymbol[Actor1](field.symbol.asMethodSymbol.name.toString), field.init) :: endStates
        })
        var endMethods: List[Method[_, _]] = List()
        var mainAlgo: Algo[Unit] = Forever(ScalaCode(code"()"))
        clasz.methods.map(method => {
          val cde = method.body
          //TODO: give a better way to label the main loop method
          if (method.symbol.asMethodSymbol.name.toString() == "loop") {
            val algo = liftCode(cde)
            //TODO: fix this, the check doesn't work
            if (algo.isInstanceOf[Algo[Unit]])
              mainAlgo = liftCode(cde).asInstanceOf[Algo[Unit]]
            println(algo)
            //TODO: else, throw an exception? or make it so that main in deep.Simulation can be of type Algo[Any]
          } else {
            println(liftCode(cde))
            //TODO: make it recognize the type of method and make the body of method viable, maybe liftCode should also return a variable/tuple of variables?
            endMethods = LocalMethod(ecosim.deep.IR.methodSymbol[Actor1](method.symbol.asMethodSymbol.name.toString), (variable1: Variable[Int]) => ScalaCode(code"""println("test")""")) :: endMethods
          }
        })
        //TODO: fix the type param of ActorType.. why doesnt this work?
        //      val a: clasz.C.Typ = new Actor1()
        //      println(a)
        endTypes = ActorType[Actor1](clasz.name, endStates, endMethods, mainAlgo, clasz.self.asInstanceOf[Variable[Actor1]]) :: endTypes
      }
    })
    ecosim.deep.Simulation(endTypes, actorsInit)
  }

  def liftCode[T: CodeType](cde: OpenCode[T]): Algo[T] = {
//    base.debugFor(
    cde match {
      case code"val $x: $xt = $v; $rest: T" =>
        LetBinding2(Some(x), ScalaCode(v), liftCode(rest))
      case code"$e; $rest: T" =>
        LetBinding2(None, liftCode(e), liftCode(rest))
      case code"($x: List[$tb]).foreach[$ta](($y: tb) => $foreachbody)" =>
        val f: Foreach[tb.Typ, Unit] = Foreach(x, y, liftCode(code"$foreachbody; ()"))
        f.asInstanceOf[Algo[T]]
      case code"while(true) $body" =>
        val f = Forever(liftCode(body))
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
  val cls2: ClassWithObject[MainClass] = MainClass.reflect(IR)
  val lifter = new Lifter()
  val simu = lifter(List(cls1, cls2))
//  val m = cls1.methods.head
//  lifter.liftCode(m.body.asInstanceOf[OpenCode[Any]])
//  val cde1 = m.body
//  lifter.liftCode(cde1)
//
//  println()
//  println(m.body.getClass)
//  println()
//  println(cde1.getClass)
//  val cde = code"println('a');println(42);123"
//  test(cde1)
//
//  def test[T: CodeType](cde: OpenCode[T]) = {
//    cde match {
//      case code"$e; $rest: T" =>
//        //case code"$e; $rest: Int" =>
//        println("success")
//        println(e)
//        println(rest)
//      case _ =>
//        println("fail")
//    }
//  }

}
