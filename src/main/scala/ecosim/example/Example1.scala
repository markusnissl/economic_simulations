package ecosim.example.ex1

import ecosim.runtime._
import ecosim.sim

@sim
class Market extends Actor {
  var goods: List[String] = Nil
}

@sim
class Farmer(market: Market) extends Actor {
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

  val market = ActorType[Market]("Market",
    State[List[String]](IR.methodSymbol[Market]("goods"), code"Nil") :: Nil,
    Nil,
    ScalaCode(code"()"),
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
    Forever(ScalaCode(code"println(5)"), Wait(code"1")),
    farmerSelf)

  val simulation = Simulation(market :: farmer :: Nil, code"val m = new Market; List(m, new Farmer(m))")

  println(simulation)

  // Test after discussion
  def interpret[A](algo: Algo[A]): Unit = algo match {
    case ScalaCode(cde) => {
      println(cde)
    }
    case _ => {
      println("No match")
    }
  }

  val actors = simulation.init.unsafe_asClosedCode.run
  val simu = new _root_.Simulation.Simulation()
  simu.init(actors)

  actors.foreach(x => x match {
    case f: Farmer => f.algo_c = _root_.code.compile(Interpreter(farmer.main, Nil))
    case m: Market => m.algo_c = _root_.code.compile(Interpreter(market.main, Nil))
  })

  simu.run(10)

  val c: OpenCode[Int] = code"List(1,2,$farmerSelf).size"
  println(c)
  //println(c.unsafe_asClosedCode.run) // oops!
  val f: Farmer => Int = code"($farmerSelf: Farmer) => $c".unsafe_asClosedCode.run
  println(code"($farmerSelf: Farmer) => $c")
  println(f(new Farmer(new Market)))

}
