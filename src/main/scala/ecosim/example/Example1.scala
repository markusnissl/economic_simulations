package ecosim.example.ex1

import code.{SimpleInstruction, __goto, __return}
import ecosim.classLifting.{Actor1, Actor2, Lifter, MainClass}
import ecosim.deep.Interpreter.Assignment
import ecosim.runtime._
import ecosim.sim
import ecosim.deep.IR
import IR.TopLevel._

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

  def recursiveTest(l: List[Int]): Unit = l match {
    case (x::xs) => {println(x); recursiveTest(xs)}
    case Nil => {}
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

  val rFSym = IR.methodSymbol[Market]("recursiveTest")
  val recursiveFunction = NonBlockingMethod[List[Int]](rFSym, (arg: Variable[List[Int]]) => LetBinding(
    None,
    If(code"$arg.tail.isEmpty == false", CallMethod[List[Int], Unit](rFSym, code"$arg.tail")),
    ScalaCode(code"""println($arg.head);""")
  ))

  val resultMessageCall = Variable[Any]

  val p1 = Variable[_root_.Simulation.RequestMessageInter[Any, Unit]]
  val handleMessage = Foreach(
    code"$marketSelf.getRequestMessages",
    p1, LetBinding(
      Option(resultMessageCall),
      CallMethodC[Any, Any](code"$p1.methodId", code"$p1.arg"),
      ScalaCode(code"""$p1.reply($marketSelf, $resultMessageCall)""")
    )
  )

  val bindingTest = Variable[Int]

  val market = ActorType[Market]("Market",
    State[List[String]](IR.methodSymbol[Market]("goods"), code"Nil") :: Nil,
    marketSell :: marketSellB :: recursiveFunction :: Nil,
    LetBinding(Option(bindingTest), ScalaCode[Int](code"0"),
      Forever(
        LetBinding(None, handleMessage,
          LetBinding(None,
        LetBinding(Option(bindingTest), ScalaCode[Int](code"$bindingTest + 1"), ScalaCode(code"""println("Binding test:",$bindingTest)""")),
            LetBinding(None,
        CallMethod[Int, Int](marketSell.sym, code"10"),
              LetBinding(None,
                CallMethod[List[Int], Unit](recursiveFunction.sym, code"List(10,20,30)"),
        Wait(code"1")
              )
            )
          )
        )
      )
    ),
    marketSelf)

  val farmerSelf = Variable[Farmer]
  val testResult = Variable[Int]

  val notifySym = BlockingMethod(IR.methodSymbol[Farmer]("tell"),
    (arg: Variable[(Actor, Int)]) => ScalaCode(code"$farmerSelf.happiness -= $arg._2"))


  val p2 = Variable[Farmer]
  val farmer = ActorType[Farmer]("Farmer",
    State[Int](IR.methodSymbol[Farmer]("happiness"), code"0") ::
      State[List[Farmer]](IR.methodSymbol[Farmer]("peers"), code"Nil") :: Nil,
    notifySym :: NonBlockingMethod(IR.methodSymbol[Farmer]("notifyPeers"),
      (arg: Variable[Unit]) =>
        Foreach[Farmer, Unit](code"$farmerSelf.peers", p2,
          Send[(Actor, Int), Unit](code"$farmerSelf",code"$p2", Message(notifySym, code"($farmerSelf, $farmerSelf.happiness)"))
        )
    ) :: Nil,
    Forever(
      LetBinding(None,
      LetBinding[Int, Unit](Option(testResult), Send[Int, Int](code"$farmerSelf", code"$farmerSelf.market", Message(marketSellB, code"500")), ScalaCode(code"""println("TEST_VAR",$testResult)""")),
      Wait(code"1")
      )
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



  val p3 = Variable[Int]
  val simpleSimSelf = Variable[SimpleSim]
  val simpleSimType = ActorType(
    "SimpleSim",
    Nil,
    marketSell :: marketSellB :: Nil,
    Forever(
      LetBinding(None,
      ScalaCode(code"println(1)"),
        LetBinding(None,
          ScalaCode(code"println(2)"),
          LetBinding(None,
            Foreach(code"List(1,2,3)", p3, ScalaCode(code"println($p3)")),
            LetBinding(None,
              CallMethod[Int, Unit](marketSell.sym, code"1000"),
              LetBinding(None,
                Wait(code"1"),
                LetBinding(None,
                  LetBinding(Option(testResult), ScalaCode[Int](code"42"), ScalaCode(code"println($testResult)")),
                  LetBinding(None,
                    LetBinding(Option(testResult), ScalaCode[Int](code"11"), ScalaCode(code"println($testResult)")),
      LetBinding(Option(testResult), CallMethod[Int, Int](marketSellB.sym, code"422"), ScalaCode(code"println($testResult)"))
                  )
                )
              )
            )
          )
        )
      )
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
