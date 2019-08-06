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
    println("sell:")
    println("Market sells: " + unit)
  }

  def sell2(unit: Int, products: List[Int]): Unit = {
    println("sell2:", unit)
    products.foreach(println)
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

object ManualEmbedding extends App {

  import ecosim.deep._
  import IR.Predef._

  val marketSell = NonBlockingMethod[Int](IR.methodSymbol[Market]("sell"), (arg: Variable[Int]) => ScalaCode(code"""println("Market sells: " + $arg)"""))
  val marketSellB = BlockingMethod[Int, Boolean](IR.methodSymbol[Market]("sell"), (arg: Variable[Int]) => ScalaCode(code"""println("Market sells: " + $arg); true"""))
  val marketSell2 = NonBlockingMethod[(Int, List[Int])](IR.methodSymbol[Market]("sell2"), (arg: Variable[(Int, List[Int])]) => ScalaCode(code"""println("sell2:", $arg._1); $arg._2.foreach(println);"""))

  val marketSelf = Variable[Market]

  val resultMessageCall = Variable[Any]

  val handleMessage = Foreach(
    code"$marketSelf.getRequestMessages",
    (p: Variable[_root_.Simulation.RequestMessageInter[Any, Unit]]) => LetBinding2(
      resultMessageCall,
      CallMethodC[Any, Any](code"$p.sym", code"$p.arg"),
      ScalaCode(code"""$p.reply($marketSelf, $resultMessageCall)""")
    )
  )

  val bindingTest = Variable[Int]

  val market = ActorType[Market]("Market",
    State[List[String]](IR.methodSymbol[Market]("goods"), code"Nil") :: Nil,
    marketSell :: marketSellB :: marketSell2 :: Nil,
    LetBinding(bindingTest, code"0",
      Forever(
        handleMessage,
        LetBinding(bindingTest, code"$bindingTest + 1", ScalaCode(code"""println("Binding test:",$bindingTest)""")),
        CallMethod[Int, Boolean](marketSellB.sym, code"10"),
        CallMethod[(Int, List[Int]), Unit](marketSell2.sym, code"(10, List(1,2,3))"),
        Wait(code"1")
      )
    ),
    marketSelf)

  val farmerSelf = Variable[Farmer]
  val testResult = Variable[Boolean]

  val notifySym = BlockingMethod(IR.methodSymbol[Farmer]("tell"),
    (arg: Variable[(Actor, Int)]) => ScalaCode(code"$farmerSelf.happiness -= $arg._2"))


  val farmer = ActorType[Farmer]("Farmer",
    State[Int](IR.methodSymbol[Farmer]("happiness"), code"0") ::
      State[List[Farmer]](IR.methodSymbol[Farmer]("peers"), code"Nil") :: Nil,
    NonBlockingMethod(IR.methodSymbol[Farmer]("notifyPeers"),
      (arg: Variable[Unit]) =>
        Foreach[Farmer, Unit](code"$farmerSelf.peers", (p: Variable[Farmer]) =>
          Send[(Actor, Int), Unit](code"$p", Message(notifySym, code"($farmerSelf, $farmerSelf.happiness)"))
        )
    ) :: Nil,
    Forever(
      LetBinding2[Boolean, Unit](testResult, Send[Int, Boolean](code"$farmerSelf.market", Message(marketSellB, code"500")), ScalaCode(code"""println("TEST_VAR",$testResult)""")),
      Wait(code"1")
    ),
    farmerSelf)

  val actorTypes: List[ActorType[_]] = market :: farmer :: Nil
  val simulation = Simulation(actorTypes, code"val m = new Market; List(m, new Farmer(m))")

  val actors = simulation.compile()

  val simu = new _root_.Simulation.Simulation()
  simu.init(actors)
  simu.run(7)

}
