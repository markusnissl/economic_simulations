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
    happiness -= 1
  }
  
  def notifyPeers(): Unit = {
    peers.foreach { p =>
      p.tell(this, happiness)
    }
  }
  
}

object ManualEmbedding extends App {
  import ecosim.deep._
  import squid.IR
  import squid.IR.Predef._
  
  //val c: ClosedCode[Double] = code"1.toDouble"
  //println(c)
  //println(codeTypeOf[List[Double]])
  
  val market = ActorType[Market]("Market", State[List[String]](IR.methodSymbol[Market]("goods"), code"Nil") :: Nil, Nil, Variable[Market])
  
  val notifySym = IR.methodSymbol[Farmer]("tell")
  
  val farmerSelf = Variable[Farmer]
  
  val farmer = ActorType[Farmer]("Farmer",
    State[Int](IR.methodSymbol[Farmer]("happiness"), code"0") ::
      State[List[Farmer]](IR.methodSymbol[Farmer]("peers"), code"Nil") :: Nil,
    NonBlockingMethod(IR.methodSymbol[Farmer]("notifyPeers"),
      Foreach[Farmer](code"$farmerSelf.peers", (p: Variable[Farmer]) =>
        Send[Unit](code"$p", Message[(Actor,Int),Unit](notifySym, code"($farmerSelf, $farmerSelf.happiness)"))
      )
    ) :: Nil,
    farmerSelf)
  
  val simulation = Simulation(market :: farmer :: Nil, code"val m = new Market; List(m, new Farmer(m))")
  
  println(simulation)
  
  val c: OpenCode[Int] = code"List(1,2,$farmerSelf).size"
  println(c)
  //println(c.unsafe_asClosedCode.run) // oops!
  val f: Farmer => Int = code"($farmerSelf: Farmer) => $c".unsafe_asClosedCode.run
  println(code"($farmerSelf: Farmer) => $c")
  println(f(new Farmer(new Market)))
  
}
