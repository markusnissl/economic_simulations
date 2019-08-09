package ecosim.classLifting

import ecosim.deep._
import ecosim.deep.IR
import IR.Predef._
import IR.TopLevel._

class Lifter {
  def apply(startClasses: List[Clasz[_]]) = {
    var endTypes: List[ActorType[_]] = List()
    startClasses.foreach((clasz: Clasz[_]) => {
      var endStates: List[State[_]] = List()
      //TODO check if this works well, seems like theres something wrong with the type of created states
      clasz.fields.foreach(field => {
        endStates = State(IR.MtdSymbol(field.symbol), field.init) :: endStates
        //endStates = State(IR.methodSymbol[Actor1](field.symbol.asMethodSymbol.name.toString), field.init) :: endStates
      })
      var endMethods: List[Method[_, _]] = List()
      clasz.methods.foreach(method => {
        val cde = method.body
        println(liftCode(cde))
        //val variable1 = Variable[Int]
        endMethods = LocalMethod(ecosim.deep.IR.methodSymbol[Actor1](method.symbol.asMethodSymbol.name.toString), (variable1: Variable[Int]) => ScalaCode(code"""println("test")""")) :: endMethods
      })
      //TODO: fix the type param of ActorType
      endTypes = ActorType[Actor1](clasz.name, endStates, endMethods, Forever(endMethods.head.asInstanceOf[LocalMethod[Int, _]].body(Variable[Int])), clasz.self.asInstanceOf[Variable[Actor1]]) :: endTypes
    })
    //TODO: return a simulation with all actor types,
    //TODO: initialization of actors
    ecosim.deep.Simulation(endTypes, code"val a = new Actor1; List(a)")
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
