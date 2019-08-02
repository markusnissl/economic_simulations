package ecosim.deep

import _root_.Simulation.{RequestMessageInter, ResponseMessageInter, SimO}
import code.{Instruction, __do, __doblock, __forever, __if, __wait, __dowhile}
import ecosim.runtime.Actor
import ecosim.example.ex1.Market

object Interpreter {

  import IR.Predef._

  case class Assignment[V: CodeType](v: Variable[V], var arg: V)(implicit val V: CodeType[V])

  // TODO: redefine callback handling for not passing callback forward where not needed
  def apply[A: CodeType](algo: Algo[A], ass: List[Assignment[_]], callback: (A => Unit) = null): (Instruction, List[Assignment[_]]) = algo match {
    case Forever(bdy@_*) => {
      var l = List[Instruction]()

      var newAss = ass
      for (el <- bdy) {
        var x = apply(el, newAss, null)
        newAss = x._2
        l = List(x._1) ::: l
      }

      (__forever(l.reverse: _*),ass)
    }
    case Block(bdy@_*) => {
      var l = List[Instruction]()

      var newAss = ass
      for (el <- bdy) {
        var x = apply(el, newAss, null)
        newAss = x._2
        l = List(x._1) ::: l
      }

      (__doblock(l.reverse: _*),ass)
    }
    case Wait(cde) => {
      (__wait(bindAll(ass, cde).evalClosed),ass)
    }
    case cM: CallMethodC[b, c] => {
      import cM.E
      val arg = bindAll(ass, cM.arg).evalClosed
      val meth = bindAll(ass, cM.mtd).evalClosed

      val v = Variable[b]
      val mBody: Algo[A] = meth.body(v)

      apply(mBody, Assignment(v, arg) :: ass, (value:A) => {
        println("Got result", value)
      })
    }
    case cM: CallMethod[b, c] => {
      import cM.E

      val arg = bindAll(ass, cM.arg).evalClosed
      val v = Variable[b]
      val mBody: Algo[A] = cM.mtd.body(v)

      apply(mBody, Assignment(v, arg) :: ass, (value:A) => {
        println("Got result", value)
      })
    }
    case send: Send[b, c] => {
      import send.E

      var blocking = false
      var requestMessage: RequestMessageInter[_, _] = null
      var sender: SimO = null
      var responseMessage: ResponseMessageInter[A, _] = null

      var command = __doblock(
        __do {
          val receiver: Actor = bindAll(ass, send.actorRef).evalClosed
          val tmpVar = ass.head.v.asInstanceOf[Variable[Actor]]
          sender = bindAll(ass, code"$tmpVar").evalClosed

          val arg = bindAll(ass, send.msg.arg).evalClosed

          requestMessage = RequestMessageInter(sender.id, receiver.id, send.msg.mtd, arg)
          sender.sendMessage(requestMessage)

          if (send.msg.mtd.isInstanceOf[BlockingMethod[_, _]]) {
            blocking = true
          } else {
            blocking = false
          }
          blocking = true
        },
        __if(blocking) (
          __do {
            sender.setMessageResponseHandler(requestMessage.sessionId, (response: _root_.Simulation.Message) => {
              responseMessage = response.asInstanceOf[ResponseMessageInter[A,_]]
            })
          },
          __dowhile(__wait(1))(responseMessage == null),
          __do {
            println("Got response!", responseMessage.arg)
            if (callback != null) {
              callback(responseMessage.arg)
            }
          }
        )
      )

      (command, ass)
    }
    case fe: Foreach[b, A] =>
      import fe.E
      //TODO: this foreach implementation is not compatible to do the operation stepwise

      var newAss = ass

      var command = __do {
        val ls = bindAll(ass, fe.ls).evalClosed
        val v = Variable[b]
        val al:Algo[A] = fe.f(v)

        ls.foreach { e =>
          var x = apply(al, Assignment(v, e) :: newAss, null)
          // Remove added assignment for e
          newAss = x._2.tail
        }

        if (callback != null) {
          callback(())
        }
      }
      (command, newAss)
    case ScalaCode(cde) => {
      //TODO: Execute directly for now, may be in a do, try to add some ``dynamic`` steps into the compiled code
      val result = bindAll(ass, cde).evalClosed
      (__do {
        if (callback != null) {
          callback(result)
        }
      },ass)
    }
    case ScalaCodeWrapper(cde) => {
      (__do {
        bindAll(ass, cde).evalClosed
      }, ass)
    }
    case LetBinding(bound, value, body) => {
      val valueInter = bindAll(ass, value).evalClosed

      var oldAssOption = ass.find(x => x.v == bound)
      if (oldAssOption.isDefined) {
        //This does not work because of type, therefore replace assignment with new value
        //var oldAss = oldAssOption.get
        //oldAss.arg = valueInter
        var newAss = ass.map(x => (if (x.v == bound) Assignment(bound, valueInter) else x))
        apply(body, newAss, callback)
      } else {
        //Assignment only for this "block"
        var x = apply(body, Assignment(bound, valueInter) :: ass, callback)
        (x._1, ass)
      }
    }
    case lb2: LetBinding2[A,c] => {

      var valueInter: Any = null

      val algo1 = apply(lb2.value, ass, (result:A) => {
        valueInter = result
      })

      var algo2:(Instruction, List[Assignment[_]]) = null

      var oldAssOption = ass.find(x => x.v == lb2.bound)
      if (oldAssOption.isDefined) {
        //This does not work because of type, therefore replace assignment with new value
        //var oldAss = oldAssOption.get
        //oldAss.arg = valueInter
        var newAss = ass.map(x => (if (x.v == lb2.bound) Assignment(lb2.bound, valueInter.asInstanceOf[A]) else x))
        algo2 = apply(lb2.body, newAss, callback)
      } else {
        //Assignment only for this "block"
        val x = apply(lb2.body, Assignment(lb2.bound, valueInter.asInstanceOf[A]) :: ass, callback)
        algo2 = (x._1, ass)
      }

      (__doblock (algo1._1, algo2._1), algo2._2)
    }
  }


  def bindAll[A: CodeType](ass: List[Assignment[_]], cde: OpenCode[A]): Bound[A] = {
    def bindAllInner(ass: List[Assignment[_]]): Bound[A] = ass match {
      case Nil => BoundNil(cde)
      case (as: Assignment[v]) :: ass =>
        import as._
        BoundCons(as.v, as.arg, bindAllInner(ass))
    }
    bindAllInner(ass.reverse)
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
