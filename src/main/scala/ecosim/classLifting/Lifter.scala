package ecosim.classLifting

import ecosim.deep._
import ecosim.deep.IR
import IR.Predef._
import IR.TopLevel._

class Lifter {
  def apply(actorClasses: List[Clasz[_]]) = {
    var actorTypes: List[ActorType[_]] = List()
    actorClasses.foreach(clasz => {
      var actorMethods: List[Method[_, _]] = List()
      clasz.methods.foreach(method => {
        val cde = method.body
        println(liftCode(cde))
        //val variable1 = Variable[Int]
        actorMethods = LocalMethod(ecosim.deep.IR.methodSymbol[Actor1](method.symbol.asMethodSymbol.name.toString), (variable1: Variable[Int]) => ScalaCode(code"""println("test")""")) :: actorMethods
      })
      actorTypes = ActorType[Actor1](clasz.name, List(), actorMethods, Forever(actorMethods.head.asInstanceOf[LocalMethod[Int, _]].body(Variable[Int])), Variable[Actor1]) :: actorTypes
    })
    //TODO: return a simulation with all actor types,
    //TODO: initialization of actors
    ecosim.deep.Simulation(actorTypes, code"val a = new Actor1; List(a)")
  }

  def liftCode[T: CodeType](cde: OpenCode[T]): Algo[T] = {
    cde match {
      case code"val $x: $xt = $v; $body: T" => LetBinding(x, v, liftCode(body))
        //TODO change foreach so that it doesnt allow any, but as a type, but rather Unit
      case code"($x: List[$tb]).foreach[$ta](($y: tb) => $foreachbody)" =>
        val f: Foreach[tb.Typ, Any] = Foreach(x, y, liftCode(foreachbody).asInstanceOf[Algo[Any]])
        f.asInstanceOf[Algo[T]]
      case code"while(true) $body" =>
        val f = Forever(liftCode(body))
        f.asInstanceOf[Algo[T]]
      case code"$e; $rest: T" =>
        println("SUCCESS")
        ScalaCode(e.asInstanceOf[OpenCode[T]])
      case _ => println("fail"); ScalaCode(cde)
    }
  }
}

//      val f: Foreach[Any, ta.Typ] = Foreach(x, (z: Variable[Any]) => {
//        val s_body = y.substitute(foreachbody.unsafe_asClosedCode, z.toCode)
//        liftCode(s_body)
//      })
//      f.asInstanceOf[Algo[T]]

object App1 extends App {
  val cls1: ClassWithObject[Actor1] = Actor1.reflect(IR)
  val lifter = new Lifter()
  val simu = lifter(List(cls1))

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
