package ecosim
package deep

import IR.Predef._
import code.{SimpleInstruction, __return}
import ecosim.deep.Interpreter.Assignment

import scala.collection.mutable.ListBuffer
import squid.lib.MutVar

case class Message[A,R](mtd: NonLocalMethod[A,R], arg: OpenCode[A])

sealed abstract class Algo[A](implicit val tpe: CodeType[A])
case class Forever(body: Algo[_]) extends Algo[Unit]
case class Wait(cde: OpenCode[Int]) extends Algo[Unit]
case class CallMethod[E, R: CodeType](sym: IR.MtdSymbol, arg: OpenCode[E])(implicit val E: CodeType[E]) extends Algo[R]
case class CallMethodC[E, R: CodeType](methodId: OpenCode[Int], arg: OpenCode[E])(implicit val E: CodeType[E]) extends Algo[R]
case class Send[E,R](actorFrom: OpenCode[runtime.Actor], actorRef: OpenCode[runtime.Actor], msg: Message[E,R])(implicit val E: CodeType[E], implicit val R:CodeType[R]) extends Algo[R]
case class Foreach[E, R: CodeType](ls: OpenCode[List[E]], variable: Variable[E], f: Algo[R])(implicit val E: CodeType[E]) extends Algo[Unit]
case class ScalaCode[A: CodeType](cde: OpenCode[A]) extends Algo[A]
case class If(cond: OpenCode[Boolean], body:Algo[Unit]) extends Algo[Unit]

/***
  * used for both bindings and sequences of Algos
  */
case class LetBinding[V: CodeType, A: CodeType](bound: Option[Variable[V]], value: Algo[V], rest: Algo[A])(implicit val V: CodeType[V]) extends Algo[A]

sealed abstract class Method[A,R](implicit val A: CodeType[A]) {
  val sym: IR.MtdSymbol
  val body: Variable[A] => Algo[R]
  override def hashCode() = sym.hashCode()
  override def equals(that: Any) = that match {
    case that: Method[_,_] => that.sym === sym
  }
  override def toString = s"${sym.asMethodSymbol.owner.name}.${sym.asMethodSymbol.name}"
}
case class LocalMethod[A,R](sym: IR.MtdSymbol, body: Variable[A] => Algo[R])(implicit override val A: CodeType[A]) extends Method[A,R]
sealed abstract class NonLocalMethod[A,R](implicit override val A: CodeType[A]) extends Method[A,R]
case class BlockingMethod[A,R](sym: IR.MtdSymbol, body: Variable[A] => Algo[R])(implicit override val A: CodeType[A]) extends NonLocalMethod[A,R]
case class NonBlockingMethod[A](sym: IR.MtdSymbol, body: Variable[A] => Algo[Unit])(implicit override val A: CodeType[A]) extends NonLocalMethod[A,Unit]

//case class State[A: CodeType](sym: IR.MtdSymbol, init: OpenCode[A])
case class State[A](sym: IR.MtdSymbol, init: OpenCode[A])(implicit val tpe: CodeType[A]) {
  override def toString = s"var ${sym.asMethodSymbol.owner.name}.${sym.asMethodSymbol.name}"
}

case class ActorType[X <: runtime.Actor](name: String, state: List[State[_]], methods: List[Method[_,_]], main: Algo[Unit], self: Variable[X])(implicit val X:CodeType[X]){

  private def compileMethod[A](method: Method[A,_], args:ListBuffer[Assignment[_]], methodIdMapping: Map[Int, IR.MtdSymbol]): (Variable[_], Vector[SimpleInstruction]) = {
    import method.A
    val methodArgs = Variable[A];
    val methodAlgo = method.body(methodArgs)
    (methodArgs, _root_.code.compile(Interpreter(methodAlgo, args, null, methodIdMapping)))
  }

  def compile(selfRef: runtime.Actor, methodIdMapping: Map[Int, IR.MtdSymbol]): (Int, Vector[SimpleInstruction]) = {
    var pos = 0
    var code: Vector[SimpleInstruction] = Vector()
    val args = ListBuffer[Assignment[_]](new Assignment(self, selfRef.asInstanceOf[X]))

    var methodPositions:Map[IR.MtdSymbol, (Int, Variable[_])] = Map()

    for (mtd <- this.methods) {
      val compiledCode = compileMethod(mtd, args, methodIdMapping)
      methodPositions = methodPositions + (mtd.sym -> (pos, compiledCode._1))

      val method = _root_.code.shift(compiledCode._2 ++ Vector[SimpleInstruction](__return()), pos)
      pos += method.length
      code = code ++ method
    }

    val main = _root_.code.shift(_root_.code.compile(Interpreter(this.main, args, methodPositions, methodIdMapping)), pos)
    code = code ++ main

    (pos, code )
  }

  // Just for testing at the moment
  def codegen(selfRef: X, methodIdMapping: Map[Int, IR.MtdSymbol]): (Int, Int, Int) => (Int, Int) = {
    val cg = new Codegen[X](methodIdMapping, this)
    cg.compile(selfRef)
  }

}

case class Simulation(actorTypes: List[ActorType[_]], init: OpenCode[List[runtime.Actor]], methodIdMapping: Map[Int, IR.MtdSymbol]) {

  def compile(): List[runtime.Actor] = {
    val actors = this.init.unsafe_asClosedCode.run
    actors.foreach(a => {
      val actorTypeSearch = actorTypes.find(_.name == a.getClass.getSimpleName)
      if (actorTypeSearch.isEmpty) {
        throw new Exception("Actor Type not defined")
      }

      val actorType = actorTypeSearch.get

      val info = actorType.compile(a, methodIdMapping)

      //TODO: handle states
      /*actorType.state.foreach(s => {
        println(s.sym, s.init)
      })*/

      a.main_pos = info._1
      a.algo_c = info._2
    })

    actors
  }

  def codegen(): List[runtime.Actor] = {
    val actors = this.init.unsafe_asClosedCode.run
    actors.foreach(a => {
      val actorTypeSearch = actorTypes.find(_.name == a.getClass.getSimpleName)
      if (actorTypeSearch.isEmpty) {
        throw new Exception("Actor Type not defined")
      }

      val actorType = actorTypeSearch.get

      actorType match {
        case aT:ActorType[c] => {
          import aT.X
          a.stepFunction = actorType.asInstanceOf[ActorType[c]].codegen(a.asInstanceOf[c], methodIdMapping)
          a.useStepFunction = true
        }

      }


    })

    actors
  }

}
