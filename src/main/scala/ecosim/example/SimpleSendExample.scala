package ecosim.example

import ecosim.deep.IR
import IR.TopLevel._
import ecosim.classLifting.{Lifter, NBUnit}
import squid.quasi.lift
import ecosim.classLifting.SpecialInstructions._
import ecosim.deep.codegen.{ActorMerge, CreateActorGraphs, CreateCode, GraphMerge, Pipeline, SSO}
import ecosim.deep.member.Actor

@lift
class liftBrostateless extends Actor {
  def getValue(i: Int) = 5 + i
  def main(): Unit = {
    while (true) {
      handleMessages()
      waitTurns(1)
    }
  }
}

@lift
class ActorReceiverCubedstateless extends Actor {
  def getValue(i: Int)(j: Int): Int = {
    if (j > 2) getValue(i)(j - 1)
    else i + 8
  }
  var env4 = new liftBrostateless()
  def main(): Unit = {
    while (true) {
      handleMessages()
      waitTurns(1)
    }
  }
}

@lift
class ActorReceiverSquaredstateless extends Actor {
  def local2() = 131313
  def getValue(i: Int) = env3.getValue(i)(5) + env3.getValue(i)(5) + env3.getValue(1)(6)
  var env3 = new ActorReceiverCubedstateless()
  def main(): Unit = {
    while (true) {
      handleMessages()
      waitTurns(1)
    }
  }
}


@lift
class ActorReceiverstateless() extends Actor {
  def getValue(i: Int) = env2.getValue(i)
  def metBlocking(i: Int): Int = getValue(i)
  var env2 = new ActorReceiverSquaredstateless()
//  def metNonBlocking(i: Int): NBUnit = NBUnit()
  def main() = {
    while(true) {
      handleMessages()
      waitTurns(1)
    }
  }
}


@lift
class ActorSender() extends Actor {
  var env: ActorReceiverstateless = new ActorReceiverstateless()
  var a: Int = 1
  def main(): Unit = {
    while(true){
      a = env.metBlocking(3)
      println(a)
//      waitTurns(1)
    }
  }
}

@lift
class InitClass2() {
  def init() = {
    val a = new ActorSender()
    List[Actor](a, a.env, a.env.env2, a.env.env2.env3, a.env.env2.env3.env4)
//    List[Actor](a, a.env)
  }
}

//FIXME doesnt work with this class
//@lift
//class MCA() extends Actor {
//  def met(): Int = 10
//  def met2() = 15
//  def main() = {
//    met()
//    met2()
//  }
//}


object SimpleSendExample extends App {
  val cls1: ClassWithObject[ActorReceiverstateless] = ActorReceiverstateless.reflect(IR)
  val cls2: ClassWithObject[ActorSender] = ActorSender.reflect(IR)
  val cls3: ClassWithObject[InitClass2] = InitClass2.reflect(IR)
  val cls4: ClassWithObject[ActorReceiverSquaredstateless] = ActorReceiverSquaredstateless.reflect(IR)
  val cls5: ClassWithObject[ActorReceiverCubedstateless] = ActorReceiverCubedstateless.reflect(IR)
  val cls6: ClassWithObject[liftBrostateless] = liftBrostateless.reflect(IR)
  val lifter = new Lifter()
  val (actorTypes, initCode) = lifter(List(cls2, cls1, cls4, cls5, cls6), cls3)
  val pipeline = Pipeline(new CreateActorGraphs(actorTypes), List(
    new SSO(),
    new GraphMerge(),
    new CreateCode(initCode),
  ))

  pipeline.run()
}
