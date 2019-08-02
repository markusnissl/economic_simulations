package ecosim.deep

import _root_.Simulation.{RequestMessageInter, ResponseMessageInter, SimO}
import code.{Instruction, __do, __doblock, __forever, __if, __wait, __dowhile}
import ecosim.runtime.Actor
import ecosim.example.ex1.Market

object Interpreter {

  import IR.Predef._

  case class Assignment[V: CodeType](v: Variable[V], arg: V)(implicit val V: CodeType[V])

  def apply[A: CodeType](algo: Algo[A], ass: List[Assignment[_]]): Instruction = algo match {
    case Forever(bdy@_*) => {
      var l = List[Instruction]()
      for (el <- bdy) {
        l = List(apply(el, ass)) ::: l
      }
      __forever(l: _*)
    }
    case Block(bdy@_*) => {
      var l = List[Instruction]()
      for (el <- bdy) {
        l = List(apply(el, ass)) ::: l
      }
      __doblock(l: _*)
    }
    case Wait(cde) => {
      __wait(bindAll(ass, cde).evalClosed)
    }
    case cM: CallMethodC[b, c] => {
      import cM.E
      val arg = bindAll(ass, cM.arg).evalClosed
      val meth = bindAll(ass, cM.mtd).evalClosed

      val v = Variable[b]
      val mBody: Algo[_] = meth.body(v)
      apply(mBody, Assignment(v, arg) :: ass)
    }
    case cM: CallMethod[b, c] => {
      import cM.E

      val arg = bindAll(ass, cM.arg).evalClosed
      val v = Variable[b]
      val mBody: Algo[_] = cM.mtd.body(v)

      apply(mBody, Assignment(v, arg) :: ass)
    }
    case Send(actorRef, msg) => {
      var blocking = false
      var requestMessage: RequestMessageInter[_,_] = null
      var sender: SimO = null
      var responseMessage:ResponseMessageInter[_,_] = null
      __doblock (
        __do {
          val receiver: Actor = bindAll(ass, actorRef).evalClosed
          val tmpVar = ass.head.v.asInstanceOf[Variable[Actor]]
          sender = bindAll(ass, code"$tmpVar").evalClosed

          val arg = bindAll(ass, msg.arg).evalClosed

          requestMessage = RequestMessageInter(sender.id, receiver.id, msg.mtd, arg)
          sender.sendMessage(requestMessage)

          if (msg.isInstanceOf[BlockingMethod[_,_]]) {
            blocking = true
          } else {
            blocking = false
          }
        },
        __if(blocking) {
          __do {
            sender.setMessageHandler(requestMessage.sessionId, (response:_root_.Simulation.Message) => {
              responseMessage = response.asInstanceOf[ResponseMessageInter[Any,Any]]
            })
          }
          __dowhile(__wait(1))(responseMessage == null)
          __do {
            println("Got response!")
          }
        }
      )
    }
    case fe: Foreach[b] =>
      import fe.E
      //TODO: this foreach implementation is not compatible to do the operation stepwise
      __do {
        val ls = bindAll(ass, fe.ls).evalClosed
        val v = Variable[b]
        val al = fe.f(v)
        ls.foreach { e =>
          apply(al, Assignment(v, e) :: ass)
        }
      }
    case ScalaCode(cde) => {
      //TODO: Execute directly for now, may be in a do, try to add some ``dynamic`` steps into the compiled code
      bindAll(ass, cde).evalClosed
      __do{}
    }
    case LetBinding(bound, value, body) => {
      __do {
        println("Cannot do binding")
      }
    }
  }

  def bindAll[A: CodeType](ass: List[Assignment[_]], cde: OpenCode[A]): Bound[A] = ass match {
    case Nil => BoundNil(cde)
    case (as: Assignment[v]) :: ass =>
      import as._
      BoundCons(as.v, as.arg, bindAll(ass, cde))
  }

  sealed abstract class Bound[A] {
    type T
    implicit val T: CodeType[T]

    def cde: OpenCode[T]

    def adapt: T => A

    def evalClosed: A = adapt(cde.unsafe_asClosedCode.run)
  }

  case class BoundCons[V: CodeType, A: CodeType](v: Variable[V], arg: V, rest: Bound[A]) extends Bound[A] {
    type T = V => rest.T
    val T: CodeType[T] = implicitly

    def cde = code"($v: V) => ${rest.cde}"

    val adapt = (f: T) => rest.adapt(f(arg))
  }

  case class BoundNil[A: CodeType](c: OpenCode[A]) extends Bound[A] {
    type T = A
    val T: CodeType[T] = implicitly

    def cde = c

    def adapt: T => A = identity
  }

}

object InterpreterTest extends App {

  import IR.Predef._

  val lsc = code"List(1,2,3)"

  println(Interpreter(ScalaCode(lsc), Nil))

  println(Interpreter(Foreach(lsc, (x: Variable[Int]) => ScalaCode(code"println($x + 1)")), Nil))


}
