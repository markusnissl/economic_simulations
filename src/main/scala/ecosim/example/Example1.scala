package ecosim.example.ex1

import ecosim.deep.Interpreter.Assignment
import ecosim.runtime._
import ecosim.sim

@sim
class Market extends Actor {
  var goods: List[String] = Nil

  def sell(unit: Int): Unit = {
    println("sell:")
    println("Market sells: " + unit)
  }

  def sell2(unit: Int, products:List[Int]): Unit = {
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

  //val c: ClosedCode[Double] = code"1.toDouble"
  //println(c)
  //println(codeTypeOf[List[Double]])

  val marketSell = NonBlockingMethod[Int](IR.methodSymbol[Market]("sell"), (arg: Variable[Int]) => ScalaCode(code"""println("sell:"); println("Market sells: " + $arg)"""))
  val marketSellB = BlockingMethod[Int, Boolean](IR.methodSymbol[Market]("sell"), (arg: Variable[Int]) => ScalaCode(code"""println("sell:"); println("Market sells: " + $arg); true"""))
  val marketSell2 = NonBlockingMethod[(Int,List[Int])](IR.methodSymbol[Market]("sell2"), (arg: Variable[(Int,List[Int])]) => ScalaCode(code"""println("sell2:", $arg._1); $arg._2.foreach(println)"""))

  val marketSelf = Variable[Market]

  val handleMessage = Foreach(code"$marketSelf.getRequestMessages", (p: Variable[_root_.Simulation.RequestMessageInter[Any,Unit]]) => CallMethodC(code"$p.mtd", code"$p.arg"))

  val bindingTest = Variable[Int]

  val market = ActorType[Market]("Market",
    State[List[String]](IR.methodSymbol[Market]("goods"), code"Nil") :: Nil,
    Nil,
    //Forever(handleMessage, CallMethod[Int, Unit](marketSell, code"10"),CallMethod(marketSell2, code"(10, List(1,2,3))"),Wait(code"1")),
    Forever(handleMessage, LetBinding(bindingTest, code"$bindingTest + 1", ScalaCodeWrapper(code"""println("Binding test:",$bindingTest)""")), CallMethod[Int, Boolean](marketSellB, code"10"),CallMethod(marketSell2, code"(10, List(1,2,3))"),Wait(code"1")),
    Variable[Market])

  val farmerSelf = Variable[Farmer]

  val notifySym = BlockingMethod(IR.methodSymbol[Farmer]("tell"),
    (arg: Variable[(Actor, Int)]) => ScalaCode(code"$farmerSelf.happiness -= $arg._2"))


  val farmer = ActorType[Farmer]("Farmer",
    State[Int](IR.methodSymbol[Farmer]("happiness"), code"0") ::
      State[List[Farmer]](IR.methodSymbol[Farmer]("peers"), code"Nil") :: Nil,
    NonBlockingMethod(IR.methodSymbol[Farmer]("notifyPeers"),
      (arg: Variable[Unit]) =>
        Foreach[Farmer](code"$farmerSelf.peers", (p: Variable[Farmer]) =>
          Send[Unit](code"$p", Message[(Actor, Int), Unit](notifySym, code"($farmerSelf, $farmerSelf.happiness)"))
        )
    ) :: Nil,
    Forever(ScalaCode(code"println(5)"), Send(code"$farmerSelf.market", Message[Int, Unit](marketSell, code"500")), Wait(code"1")),
    //Forever(ScalaCode(code"println(5)"), Send(code"$farmerSelf.market", Message[(Int,List[Int]), Unit](marketSell2, code"(10,List(1,2,3))")), Wait(code"1")),
    farmerSelf)

  val simulation = Simulation(market :: farmer :: Nil, code"val m = new Market; List(m, new Farmer(m))")

  println(simulation)

  val actors = simulation.init.unsafe_asClosedCode.run
  val simu = new _root_.Simulation.Simulation()
  simu.init(actors)

  actors.foreach(x => x match {
    case f: Farmer =>  {
      f.algo_c = _root_.code.compile(Interpreter(farmer.main, Assignment(farmerSelf, f) :: Nil))
    }
    case m: Market => {
      m.algo_c = _root_.code.compile(Interpreter(market.main, Assignment(bindingTest, 0) :: Assignment(marketSelf, m) :: Nil))
    }
  })

  simu.run(5)

  val c: OpenCode[Int] = code"List(1,2,$farmerSelf).size"
  println(c)
  //println(c.unsafe_asClosedCode.run) // oops!
  val f: Farmer => Int = code"($farmerSelf: Farmer) => $c".unsafe_asClosedCode.run
  println(code"($farmerSelf: Farmer) => $c")
  println(f(new Farmer(new Market)))
}
