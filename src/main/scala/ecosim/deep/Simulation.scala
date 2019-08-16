package ecosim
package deep

import IR.Predef._

case class Message[R](mtd: LiftedMethod[R], argss: List[List[OpenCode[_]]])

sealed abstract class Algo[A](implicit val tpe: CodeType[A])
case class Forever(body: Algo[_]) extends Algo[Unit]
case class Wait(cde: OpenCode[Int]) extends Algo[Unit]
case class CallMethodDebug[R: CodeType](sym: IR.MtdSymbol, argss: List[List[OpenCode[_]]]) extends Algo[R]
case class CallMethod[R: CodeType](methodId: Int, argss: List[List[OpenCode[_]]]) extends Algo[R]
case class Send[R](actorFrom: OpenCode[runtime.Actor], actorRef: OpenCode[runtime.Actor], msg: Message[R])(implicit val R:CodeType[R]) extends Algo[R]
case class Foreach[E, R: CodeType](ls: OpenCode[List[E]], variable: Variable[E], f: Algo[R])(implicit val E: CodeType[E]) extends Algo[Unit]
case class ScalaCode[A: CodeType](cde: OpenCode[A]) extends Algo[A]
case class NoOp() extends Algo[Any]
case class If[A](cond: OpenCode[Boolean], body:Algo[A])(implicit val A: CodeType[A]) extends Algo[A]
case class IfElse[A](cond: OpenCode[Boolean], ifBody:Algo[A], elseBody:Algo[A])(implicit val A: CodeType[A]) extends Algo[A]
case class LetBinding[V: CodeType, A: CodeType](bound: Option[Variable[V]], value: Algo[V], rest: Algo[A])(implicit val V: CodeType[V]) extends Algo[A]

abstract class LiftedMethod[R](val cls: IR.TopLevel.Clasz[_], val body: Algo[R], val blocking: Boolean) {
  def sym: IR.MtdSymbol = mtd.symbol
  val mtd: cls.Method[R, cls.Scp]


  override def hashCode() = sym.hashCode()
  override def equals(that: Any) = that match {
    case that: LiftedMethod[_] => that.sym === sym
  }
  override def toString = s"${sym.asMethodSymbol.owner.name}.${sym.asMethodSymbol.name}"

}



//case class State[A: CodeType](sym: IR.MtdSymbol, init: OpenCode[A])
case class State[A](sym: IR.MtdSymbol, init: OpenCode[A])(implicit val tpe: CodeType[A]) {
  override def toString = s"var ${sym.asMethodSymbol.owner.name}.${sym.asMethodSymbol.name}"
}

case class ActorType[X <: runtime.Actor](name: String, state: List[State[_]], methods: List[LiftedMethod[_]], main: Algo[Unit], self: Variable[X])(implicit val X:CodeType[X]){

  private var stepFunction: (X) => (Int, Int, Int) => (Int, Int) = _
  /*private def compileMethod[A](method: LiftedMethod[_], args:ListBuffer[Assignment[_]], methodIdMapping: Map[Int, IR.MtdSymbol]): (Variable[_], Vector[SimpleInstruction]) = {
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
  }*/

  // Just for testing at the moment
  def codegen(methodIdMapping: Map[IR.MtdSymbol, Int]): Unit = {
    val cg = new Codegen[X](methodIdMapping, this)
    stepFunction = cg.compile
  }

  def getStepFunction(actor: X): (Int, Int, Int) => (Int, Int) = {
    stepFunction(actor)
  }

}

case class Simulation(actorTypes: List[ActorType[_]], init: OpenCode[List[runtime.Actor]], methodIdMapping: Map[IR.MtdSymbol, Int]) {

  /*def compile(): List[runtime.Actor] = {
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
  }*/

  def codegen(): List[runtime.Actor] = {
    //Generate step function of actorTypes
    actorTypes.foreach(_.codegen(methodIdMapping))

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
          a.stepFunction = actorType.asInstanceOf[ActorType[c]].getStepFunction(a.asInstanceOf[c])
          a.useStepFunction = true
        }

      }
    })

    actors
  }

}
