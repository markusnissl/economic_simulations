//package ecosim.deep
//
//import _root_.Simulation.{RequestMessageInter, ResponseMessageInter, SimO}
//import code.{Instruction, __do, __doRuntime, __doblock, __dowhile, __forever, __if, __wait}
//import ecosim.runtime.Actor
//import ecosim.example.ex1.Market
//
//import scala.collection.mutable
//
//object Interpreter {
//
//  import IR.Predef._
//
//  class Assignment[V: CodeType](val v: Variable[V], arg: => V)(implicit val V: CodeType[V]) {
//    def getArg: V = arg
//
//    override def toString: String = "Assignment(" + v + "," + arg + ")"
//  }
//
//  // TODO: redefine callback handling for not passing callback forward where not needed
//  def apply[A: CodeType](algo: Algo[A], ass: mutable.ListBuffer[Assignment[_]], callback: (A => Unit) = null): Instruction = algo match {
//    case Forever(bdy@_*) => {
//      var l = List[Instruction]()
//
//      for (el <- bdy) {
//        l = apply(el, ass, null) :: l
//      }
//
//      __forever(l.reverse: _*)
//    }
////    case Block(bdy@_*) => {
////      var l = List[Instruction]()
////
////      for (el <- bdy) {
////        l = apply(el, ass, null) :: l
////      }
////
////      __doblock(l.reverse: _*)
////    }
//    case Wait(cde) => {
//      __wait(bindAll(ass.toList, cde).evalClosed)
//    }
//    case cM: CallMethodC[b, c] => {
//      import cM.E
//
//      val v = Variable[b]
//      var mBody: Algo[A] = null
//
//      __doblock(
//        __do {
//          val meth = bindAll(ass.toList, cM.mtd).evalClosed
//          mBody = meth.body(v)
//
//          val arg = bindAll(ass.toList, cM.arg).evalClosed
//          ass.prepend(new Assignment(v, arg))
//        },
//        __doRuntime(
//          () => apply(mBody, ass, (value: A) => {
//            println("Got result CMC", value)
//          })
//        ),
//        __do {
//          ass.remove(0)
//        }
//      )
//    }
//    case cM: CallMethod[b, c] => {
//      import cM.E
//
//      val v = Variable[b]
//      val mBody: Algo[A] = cM.mtd.body(v)
//
//      __doblock(
//        __do {
//          val arg = bindAll(ass.toList, cM.arg).evalClosed
//          ass.prepend(new Assignment(v, arg))
//        },
//        apply(mBody, ass, (value: A) => {
//          println("Got result", value)
//        }),
//        __do {
//          ass.remove(0)
//        }
//      )
//    }
//    case send: Send[b, c] => {
//      import send.E
//
//      var blocking = false
//      var requestMessage: RequestMessageInter[_, _] = null
//      var sender: SimO = null
//      var responseMessage: ResponseMessageInter[A, _] = null
//
//
//      var command = __doblock(
//        __do {
//          val receiver: Actor = bindAll(ass.toList, send.actorRef).evalClosed
//          val tmpVar = ass.head.v.asInstanceOf[Variable[Actor]]
//          sender = bindAll(ass.toList, code"$tmpVar").evalClosed
//
//          val arg = bindAll(ass.toList, send.msg.arg).evalClosed
//
//          requestMessage = RequestMessageInter(sender.id, receiver.id, send.msg.mtd, arg)
//          sender.sendMessage(requestMessage)
//
//          if (send.msg.mtd.isInstanceOf[BlockingMethod[_, _]]) {
//            blocking = true
//          } else {
//            blocking = false
//          }
//          blocking = true
//        },
//        __if(blocking)(
//          __do {
//            sender.setMessageResponseHandler(requestMessage.sessionId, (response: _root_.Simulation.Message) => {
//              responseMessage = response.asInstanceOf[ResponseMessageInter[A, _]]
//            })
//          },
//          __dowhile(__wait(1))(responseMessage == null),
//          __do {
//            println("Got response!", responseMessage.arg)
//            if (callback != null) {
//              callback(responseMessage.arg)
//            }
//            responseMessage = null
//          }
//        )
//      )
//
//      command
//    }
//    case fe: Foreach[b, A] =>
//      import fe.E
//
//      var iter:Iterator[b] = null
//      val v = Variable[b]
//      val al: Algo[A] = fe.f
//
//
//      var command = __doblock(
//        __do {
//          val ls = bindAll(ass.toList, fe.ls).evalClosed
//          iter = ls.iterator
//
//        },
//        __dowhile(
//          __if(iter.hasNext)(
//            __do {
//              val e = iter.next()
//              ass.prepend(new Assignment(v, e))
//            },
//            apply(al, ass, null),
//            __do {
//              ass.remove(0)
//            }
//          )
//        )(iter.hasNext),
//        __do {
//          if (callback != null) {
//            callback(())
//          }
//        }
//      )
//      command
//    case ScalaCode(cde) => {
//      __do {
//        val result = bindAll(ass.toList, cde).evalClosed
//        if (callback != null) {
//          callback(result)
//        }
//      }
//    }
//    case LetBinding(bound, value, body) => {
//      var valueInter: Any = null
//
//      val oldAssOption = ass.find(x => x.v == bound)
//
//      val algo1 = __do {
//        valueInter = bindAll(ass.toList, value).evalClosed
//        if (oldAssOption.isDefined) {
//          val index = ass.indexWhere(_.v == bound)
//          ass.update(index, new Assignment(bound, valueInter))
//        } else {
//          ass.prepend(new Assignment(bound, valueInter))
//        }
//      }
//
//      var algo2: Instruction = null
//
//      if (oldAssOption.isDefined) {
//        algo2 = apply(body, ass, callback)
//      } else {
//        algo2 = __doblock(
//          apply(body, ass, callback),
//          __do {
//            // Remove added element again
//            ass.remove(0)
//          }
//        )
//      }
//
//      __doblock(algo1, algo2)
//    }
//
//    case lb2: LetBinding2[A, c] => {
//
//      var valueInter: Any = null
//      var oldAssOption = ass.find(x => x.v == lb2.bound)
//
//      val algo1 = __doblock(
//        apply(lb2.value, ass, (result: A) => {
//          valueInter = result
//        }),
//        __do {
//          if (oldAssOption.isDefined) {
//            val index = ass.indexWhere(_.v == lb2.bound)
//            ass.update(index, new Assignment(lb2.bound, valueInter.asInstanceOf[A]))
//          } else {
//            ass.prepend(new Assignment(lb2.bound, valueInter.asInstanceOf[A]))
//          }
//        }
//      )
//
//      var algo2: Instruction = null
//
//      if (oldAssOption.isDefined) {
//        algo2 = apply(lb2.body, ass, callback)
//      } else {
//        algo2 = __doblock(
//          apply(lb2.body, ass, callback),
//          __do {
//            // Remove added element again
//            ass.remove(0)
//          }
//        )
//      }
//
//      __doblock(algo1, algo2)
//    }
//  }
//
//
//  def bindAll[A: CodeType](ass: List[Assignment[_]], cde: OpenCode[A]): Bound[A] = {
//    def bindAllInner(ass: List[Assignment[_]]): Bound[A] = ass match {
//      case Nil => BoundNil(cde)
//      case (as: Assignment[v]) :: ass =>
//
//        import as._
//
//        BoundCons(as.v, as.getArg, bindAllInner(ass))
//    }
//
//    bindAllInner(ass.reverse)
//  }
//
//  sealed abstract class Bound[A] {
//    type T
//    implicit val T: CodeType[T]
//
//    def cde: OpenCode[T]
//
//    def adapt: T => A
//
//    def evalClosed: A = adapt(cde.unsafe_asClosedCode.run)
//  }
//
//  case class BoundCons[V: CodeType, A: CodeType](v: Variable[V], arg: V, rest: Bound[A]) extends Bound[A] {
//    type T = V => rest.T
//    val T: CodeType[T] = implicitly
//
//    def cde = code"($v: V) => ${rest.cde}"
//
//    val adapt = (f: T) => rest.adapt(f(arg))
//  }
//
//  case class BoundNil[A: CodeType](c: OpenCode[A]) extends Bound[A] {
//    type T = A
//    val T: CodeType[T] = implicitly
//
//    def cde = c
//
//    def adapt: T => A = identity
//  }
//
//}
//
//object InterpreterTest extends App {
//
//  import IR.Predef._
//
//  val lsc = code"List(1,2,3)"
//
//  //println(Interpreter(ScalaCode(lsc), Nil))
//
//  //println(Interpreter(Foreach(lsc, (x: Variable[Int]) => ScalaCode(code"println($x + 1)")), Nil))
//
//
//}
