package ecosim.deep

import _root_.Simulation.{RequestMessageInter, ResponseMessageInter, SimO}
import code.{Instruction, __call, __do, __doResult, __doblock, __dowhile, __forever, __goto, __if, __wait}
import ecosim.runtime.Actor
import ecosim.example.ex1.Market

import scala.collection.mutable

object Interpreter {

  import IR.Predef._
  import squid.lib.MutVar

  class Assignment[V: CodeType](val v: Variable[V], arg: => V)(implicit val V: CodeType[V]) {
    def getArg: V = arg

    override def toString: String = "Assignment(" + v + "," + arg + ")"
  }

  def apply[A: CodeType](algo: Algo[A], ass: mutable.ListBuffer[Assignment[_]], methodMapping:Map[IR.MtdSymbol, (Int,Variable[_])], methodIdMapping: Map[Int, IR.MtdSymbol]): Instruction = algo match {
    case Forever(bdy) => {
      __forever(apply(bdy, ass, methodMapping, methodIdMapping))
    }
    case Wait(cde) => {
      __wait(bindAll(ass.toList, cde).evalClosed)
    }
    case cM: CallMethodC[b, c] => {
      import cM.E

      var mtdData:(Int,Variable[_]) = null

      __doblock(
        __do {
          val methodSym:IR.MtdSymbol = methodIdMapping(cM.methodId.unsafe_asClosedCode.run)
          mtdData = methodMapping(methodSym)

          val arg = bindAll(ass.toList, cM.arg).evalClosed
          ass.prepend(new Assignment(mtdData._2.asInstanceOf[Variable[b]], arg))
        },
        __call(mtdData._1),
        __doResult{result: Any =>
          ass.remove(0)
          //Forward result
          result
        }
      )
    }
    case cM: CallMethod[b, c] => {
      import cM.E

      val mtdData:(Int,Variable[_]) = methodMapping(cM.sym)

      __doblock(
        __do {
          var x = mtdData._2
          var y = x.rep
          val arg = bindAll(ass.toList, cM.arg).evalClosed
          ass.prepend(new Assignment(mtdData._2.asInstanceOf[Variable[b]], arg))
        },
        __call(mtdData._1),
        __doResult{result: Any =>
          ass.remove(0)
          //Forward result
          result
        }
      )
    }
    case send: Send[b, c] => {
      import send.E

      var blocking = false
      var requestMessage: RequestMessageInter[_, _] = null
      var sender: SimO = null
      var responseMessage: ResponseMessageInter[A] = null


      var command = __doblock(
        __do {
          val receiver: Actor = bindAll(ass.toList, send.actorRef).evalClosed
          val sender: Actor = bindAll(ass.toList,send.actorFrom).evalClosed

          val arg = bindAll(ass.toList, send.msg.arg).evalClosed


          requestMessage = RequestMessageInter[b,c](sender.id, receiver.id, methodIdMapping.map(_.swap).get(send.msg.mtd.sym).get, arg)
          sender.sendMessage(requestMessage)

          if (send.msg.mtd.isInstanceOf[BlockingMethod[_, _]]) {
            blocking = true
          } else {
            blocking = false
          }
        },
        __if(blocking)(
          __do {
            sender.setMessageResponseHandler(requestMessage.sessionId, (response: _root_.Simulation.Message) => {
              responseMessage = response.asInstanceOf[ResponseMessageInter[A]]
            })
          },
          __dowhile(__wait(1))(responseMessage == null),
          __do {
            println("Got response!", responseMessage.arg)
            val result = responseMessage.arg
            responseMessage = null
            result
          }
        )
      )

      command
    }
    case fe: Foreach[b, A] =>
      import fe.E

      var iter:Iterator[b] = null
      val v = fe.variable
      val al: Algo[A] = fe.f


      var command = __doblock(
        __do {
          val ls = bindAll(ass.toList, fe.ls).evalClosed
          iter = ls.iterator
        },
        __dowhile(
          __if(iter.hasNext)(
            __do {
              val e = iter.next()
              ass.prepend(new Assignment(v, e))
            },
            apply(al, ass, methodMapping, methodIdMapping),
            __do {
              ass.remove(0)
            }
          )
        )(iter.hasNext),
      )
      command
    case ScalaCode(cde) => {
      __do {
        bindAll(ass.toList, cde).evalClosed
      }
    }

    case lb: LetBinding[A, c] => {

      if (lb.bound.isEmpty) {
        __doblock(apply(lb.value, ass, methodMapping, methodIdMapping), apply(lb.rest, ass, methodMapping,methodIdMapping))
      } else {
        var valueInter: Any = null
        var oldAssOption: Option[Assignment[_]] = None

        val algo1 = __doblock(
          __do {
            oldAssOption = ass.find(x => x.v == lb.bound.get)
          },
          apply(lb.value, ass, methodMapping, methodIdMapping),
          __doResult { result: Any =>
            valueInter = result

            if (oldAssOption.isDefined) {
              val index = ass.indexWhere(_.v == lb.bound.get)
              ass.update(index, new Assignment(lb.bound.get, valueInter.asInstanceOf[A]))
            } else {
              ass.prepend(new Assignment(lb.bound.get, valueInter.asInstanceOf[A]))
            }
          }
        )

        var algo2: Instruction = __doblock(
          __if(oldAssOption.isDefined)(
            apply(lb.rest, ass, methodMapping, methodIdMapping)
          ),
          __if(!oldAssOption.isDefined)(
            apply(lb.rest, ass, methodMapping, methodIdMapping),
            __do {
              // Remove added element again
              ass.remove(0)
            }
          )
        )

        __doblock(algo1, algo2)
      }
    }
  }


  def bindAll[A: CodeType](ass: List[Assignment[_]], cde: OpenCode[A]): Bound[A] = {
    def bindAllInner(ass: List[Assignment[_]]): Bound[A] = ass match {
      case Nil => BoundNil(cde)
      case (as: Assignment[v]) :: ass =>

        import as._

        BoundCons(as.v, as.getArg, bindAllInner(ass))
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

  //println(Interpreter(ScalaCode(lsc), Nil))

  //println(Interpreter(Foreach(lsc, (x: Variable[Int]) => ScalaCode(code"println($x + 1)")), Nil))


}
