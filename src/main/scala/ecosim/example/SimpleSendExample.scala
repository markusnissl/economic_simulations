package ecosim.example

import ecosim.deep.IR
import IR.TopLevel._
import ecosim.classLifting.{Lifter, NBUnit}
import squid.quasi.lift
import ecosim.classLifting.SpecialInstructions._
import ecosim.deep.codegen.{CreateActorGraphs, CreateCode, GraphMerge, Pipeline, SSO}
import ecosim.deep.member.Actor

@lift
class ActorReceiver() extends Actor {
  def metBlocking(): Int = 1312
//  def metNonBlocking(i: Int): NBUnit = NBUnit()
  def main() = {
    while(true) {
      handleMessages()
      waitTurns()
    }
  }
}

//todo tell markus about not working local methods

@lift
class ActorSender() extends Actor {
  var env: ActorReceiver = new ActorReceiver()
//  def local() = 11
  def main(): Unit = {
    while(true){
      println("First command")
      env.metBlocking()
      println("next command")
      waitTurns()
    }
  }
}

@lift
class InitClass2() {
  def init() = {
    val a = new ActorSender()
    List[Actor](a, a.env)
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
  val cls1: ClassWithObject[ActorReceiver] = ActorReceiver.reflect(IR)
  val cls2: ClassWithObject[ActorSender] = ActorSender.reflect(IR)
  val cls3: ClassWithObject[InitClass2] = InitClass2.reflect(IR)
  val lifter = new Lifter()
  val (actorTypes, initCode) = lifter(List(cls1, cls2), cls3)

  val pipeline = Pipeline(new CreateActorGraphs(actorTypes), List(
    new SSO(),
    new GraphMerge(),
    new CreateCode(initCode),
  ))

  pipeline.run()
}
