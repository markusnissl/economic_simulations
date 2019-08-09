package ecosim.example.ex1

import code.{SimpleInstruction, __goto, __return}
import ecosim.deep.Interpreter.Assignment
import ecosim.runtime._
import ecosim.sim

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

@sim
class Market extends Actor {
  var goods: List[String] = Nil

  def sell(unit: Int): Unit = {
    println("Market sells: " + unit)
  }

  def sell2(unit: Int): Int = {
    println("Market sells: " + unit)
    42
  }

}

@sim
class Farmer(val market: Market) extends Actor {
  var happiness = 0
  var peers: List[Farmer] = Nil

  def tell(actor: Actor, h: Int): Unit = {
    happiness -= h
  }

  def notifyPeers(): Unit = {
    peers.foreach { p =>
      p.tell(this, happiness)
    }
  }

}

@sim
class SimpleSim() extends Actor {}

object ManualEmbedding extends App {

  import ecosim.deep._
  import IR.Predef._
  import squid.lib.MutVar

  val marketSell = NonBlockingMethod[Int](IR.methodSymbol[Market]("sell"), (arg: Variable[Int]) => ScalaCode(code"""println("Market sells: " + $arg)"""))
  val marketSellB = BlockingMethod[Int, Int](IR.methodSymbol[Market]("sell2"), (arg: Variable[Int]) => ScalaCode[Int](code"""println("Market sells: " + $arg); 42"""))

  val marketSelf = Variable[Market]

  val resultMessageCall = Variable[Any]

  val handleMessage = Foreach(
    code"$marketSelf.getRequestMessages",
    (p: Variable[_root_.Simulation.RequestMessageInter[Any, Unit]]) => LetBinding2(
      resultMessageCall,
      CallMethodC[Any, Any](code"$p.methodId", code"$p.arg"),
      ScalaCode(code"""$p.reply($marketSelf, $resultMessageCall)""")
    )
  )

  val bindingTest = Variable[Int]

  val market = ActorType[Market]("Market",
    State[List[String]](IR.methodSymbol[Market]("goods"), code"Nil") :: Nil,
    marketSell :: marketSellB :: Nil,
    LetBinding(bindingTest, code"0",
      Forever(
        handleMessage,
        LetBinding(bindingTest, code"$bindingTest + 1", ScalaCode(code"""println("Binding test:",$bindingTest)""")),
        CallMethod[Int, Int](marketSell.sym, code"10"),
        Wait(code"1")
      )
    ),
    marketSelf)

  val farmerSelf = Variable[Farmer]
  val testResult = Variable[Int]

  val notifySym = BlockingMethod(IR.methodSymbol[Farmer]("tell"),
    (arg: Variable[(Actor, Int)]) => ScalaCode(code"$farmerSelf.happiness -= $arg._2"))


  val farmer = ActorType[Farmer]("Farmer",
    State[Int](IR.methodSymbol[Farmer]("happiness"), code"0") ::
      State[List[Farmer]](IR.methodSymbol[Farmer]("peers"), code"Nil") :: Nil,
    notifySym :: NonBlockingMethod(IR.methodSymbol[Farmer]("notifyPeers"),
      (arg: Variable[Unit]) =>
        Foreach[Farmer, Unit](code"$farmerSelf.peers", (p: Variable[Farmer]) =>
          Send[(Actor, Int), Unit](code"$farmerSelf",code"$p", Message(notifySym, code"($farmerSelf, $farmerSelf.happiness)"))
        )
    ) :: Nil,
    Forever(
      LetBinding2[Int, Unit](testResult, Send[Int, Int](code"$farmerSelf", code"$farmerSelf.market", Message(marketSellB, code"500")), ScalaCode(code"""println("TEST_VAR",$testResult)""")),
      Wait(code"1")
    ),
    farmerSelf)

  val actorTypes: List[ActorType[_]] = market :: farmer :: Nil

  var methodIdMapping: Map[Int, IR.MtdSymbol] = Map()

  var counter = 0
  for (a <- actorTypes) {
    for (m <- a.methods) {
      methodIdMapping = methodIdMapping + (counter -> m.sym)
      counter = counter + 1
    }
  }

  val simulation = Simulation(actorTypes, code"val m = new Market; List(m, new Farmer(m))", methodIdMapping)

  //val actors = simulation.compile()

  /*val simu = new _root_.Simulation.Simulation()
  simu.init(actors)
  simu.run(7)*/



  val simpleSimSelf = Variable[SimpleSim]
  val simpleSimType = ActorType(
    "SimpleSim",
    Nil,
    marketSell :: marketSellB :: Nil,
    Forever(
      ScalaCode(code"println(1)"),
      ScalaCode(code"println(2)"),
      Foreach(code"List(1,2,3)", (p: Variable[Int]) => ScalaCode(code"println($p)")),
      CallMethod[Int, Unit](marketSell.sym, code"1000"),
      Wait(code"1"),
      LetBinding(testResult, code"42", ScalaCode(code"println($testResult)")),
      LetBinding2(testResult, ScalaCode[Int](code"11"), ScalaCode(code"println($testResult)")),
      LetBinding2(testResult, CallMethod[Int, Int](marketSellB.sym, code"422"), ScalaCode(code"println($testResult)"))
    ),
    simpleSimSelf
  )

  /*val cg = new Codegen[SimpleSim](methodIdMapping, simpleSimType)
  val stepFunction = cg.compile(new SimpleSim)
  stepFunction(0,0,5)*/

  val actors = simulation.codegen()
  val simu = new _root_.Simulation.Simulation()
  simu.init(actors)
  simu.run(7)

}
