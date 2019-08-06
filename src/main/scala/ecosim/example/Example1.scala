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

  val marketSell = NonBlockingMethod[Int](IR.methodSymbol[Market]("sell"), (arg: Variable[Int]) => ScalaCode(code"""println("Market sells: " + $arg)"""))
  val marketSellB = BlockingMethod[Int, Boolean](IR.methodSymbol[Market]("sell"), (arg: Variable[Int]) => ScalaCode(code"""println("Market sells: " + $arg); true"""))
  val marketSell2 = NonBlockingMethod[(Int,List[Int])](IR.methodSymbol[Market]("sell2"), (arg: Variable[(Int,List[Int])]) => ScalaCode(code"""println("sell2:", $arg._1); $arg._2.foreach(println);"""))

  val marketSelf = Variable[Market]

  val resultMessageCall = Variable[Any]

  val handleMessage = Foreach(
    code"$marketSelf.getRequestMessages",
    (p: Variable[_root_.Simulation.RequestMessageInter[Any,Unit]]) => LetBinding2(
      resultMessageCall,
      CallMethodC[Any, Any](code"$p.sym", code"$p.arg"),
      ScalaCode(code"""$p.reply($marketSelf, $resultMessageCall)""")
    )
  )

  val bindingTest = Variable[Int]

  val market = ActorType[Market]("Market",
    State[List[String]](IR.methodSymbol[Market]("goods"), code"Nil") :: Nil,
    marketSell :: marketSellB :: marketSell2 :: Nil,
    Forever(
      handleMessage,
      LetBinding(bindingTest, code"$bindingTest + 1", ScalaCode(code"""println("Binding test:",$bindingTest)""")),
      CallMethod[Int, Boolean](marketSellB.sym, code"10"),
      CallMethod[(Int, List[Int]), Unit](marketSell2.sym, code"(10, List(1,2,3))"),
      Wait(code"1")
    ),
    Variable[Market])

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
      LetBinding2[Boolean,Unit](testResult,Send[Int, Boolean](code"$farmerSelf.market", Message(marketSellB, code"500")), ScalaCode(code"""println("TEST_VAR",$testResult)""")),
      Wait(code"1")
    ),
    //Forever(Send(code"$farmerSelf.market", Message[(Int,List[Int]), Unit](marketSell2, code"(10,List(1,2,3))")), Wait(code"1")),
    farmerSelf)

  val simulation = Simulation(market :: farmer :: Nil, code"val m = new Market; List(m, new Farmer(m))")

  val actors = simulation.init.unsafe_asClosedCode.run
  val simu = new _root_.Simulation.Simulation()

  def compileMethod[A](method: Method[A,_], args:ListBuffer[Assignment[_]]): (Variable[_], Vector[SimpleInstruction]) = {
    import method.A
    val methodArgs = Variable[A];
    val methodAlgo = method.body(methodArgs)
    (methodArgs, _root_.code.compile(Interpreter(methodAlgo, args, null)))
  }


  actors.foreach(x => x match {
    case f: Farmer =>  {
      f.algo_c = _root_.code.compile(Interpreter(farmer.main, ListBuffer(new Assignment(farmerSelf, f)), null))
    }
    case m: Market => {
      val args:ListBuffer[Assignment[_]] = ListBuffer(new Assignment(bindingTest, 0), new Assignment(marketSelf, m))

      var pos = 0
      var code: Vector[SimpleInstruction] = Vector()
      var methodPositions:Map[IR.MtdSymbol, (Int, Variable[_])] = Map()

      for (mtd <- market.methods) {
        val compiledCode = compileMethod(mtd, args)
        methodPositions = methodPositions + (mtd.sym -> (pos, compiledCode._1))

        val method = _root_.code.shift(compiledCode._2 ++ Vector[SimpleInstruction](__return()), pos)
        pos += method.length
        code = code ++ method
      }

      var main = _root_.code.shift(_root_.code.compile(Interpreter(market.main, args, methodPositions)), pos)
      code = code ++ main

      // Set start pos to main´
      m.main_pos = pos
      m.algo_c = code
    }
  })

  simu.init(actors)
  simu.run(7)

  val c: OpenCode[Int] = code"List(1,2,$farmerSelf).size"
  println(c)
  //println(c.unsafe_asClosedCode.run) // oops!
  val f: Farmer => Int = code"($farmerSelf: Farmer) => $c".unsafe_asClosedCode.run
  println(code"($farmerSelf: Farmer) => $c")
  println(f(new Farmer(new Market)))

}
