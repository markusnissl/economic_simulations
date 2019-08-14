package ecosim.example.ex1

import code.{SimpleInstruction, __goto, __return}
import ecosim.classLifting.{Actor1, Actor2, Lifter, MainClass}
import ecosim.runtime._
import ecosim.sim
import ecosim.deep.IR
import IR.TopLevel._
import squid.quasi.lift

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

@lift
class Market extends Actor {
  var goods: List[String] = Nil

  def sell(unit: Int): Unit = {
    println("Market sells: " + unit)
  }

  def sell2(unit: Int): Int = {
    println("Market sells: " + unit)
    42
  }

  def recursiveTest(l: List[Int]): Unit = {
    if (l.isEmpty) {
    } else {
      recursiveTest(l.tail)
      println(l.head)
    }
  }

}

@lift
class Farmer(val market: Market) extends Actor {
  var happiness = 0
  var peers: List[Farmer] = Nil

  def tell(actor: Actor, h: Int): Unit = {
    happiness = happiness - h
  }

  def notifyPeers(): Unit = {
    peers.foreach { p =>
      p.tell(this, happiness)
    }
  }

}

@lift
class SimpleSim() extends Actor {}

object ManualEmbedding extends App {

  import ecosim.deep._
  import IR.Predef._
  import squid.lib.MutVar

  val m: ClassWithObject[Market] = Market.reflect(IR)
  val f: ClassWithObject[Farmer] = Farmer.reflect(IR)

  val mS = m.methods.find(_.symbol.asMethodSymbol.name.toString == "sell").get
  val mSB = m.methods.find(_.symbol.asMethodSymbol.name.toString == "sell2").get
  val rF = m.methods.find(_.symbol.asMethodSymbol.name.toString == "recursiveTest").get

  val te = f.methods.find(_.symbol.asMethodSymbol.name.toString == "tell").get
  val nP = f.methods.find(_.symbol.asMethodSymbol.name.toString == "notifyPeers").get

  val marketSell = new LiftedMethod[Unit](m, ScalaCode(code"""println("Market sells: " + ${mS.vparams.head.head})"""), false) {
    override val mtd: cls.Method[Unit, cls.Scp] = mS.asInstanceOf[this.cls.Method[Unit, cls.Scp]]
  }
  val marketSellB = new LiftedMethod[Int](m, ScalaCode[Int](code"""println("Market sells: " + ${mSB.vparams.head.head}); 42"""), true) {
    override val mtd: cls.Method[Int, cls.Scp] = mSB.asInstanceOf[this.cls.Method[Int, cls.Scp]]
  }
  val marketSelf = Variable[Market]

  val rFParam1:Variable[_] = rF.vparams.head.head
  val recursiveFunction = new LiftedMethod[Unit](m, LetBinding(
    None,
    If(code"${rFParam1}.asInstanceOf[List[Int]].tail.isEmpty == false", CallMethod[Unit](rF.symbol, code"List(List(${rFParam1}.asInstanceOf[List[Int]].tail))")),
    ScalaCode(code"""println(${rFParam1}.asInstanceOf[List[Int]].head);""")
  ), false) {
    override val mtd: cls.Method[Unit, cls.Scp] = rF.asInstanceOf[this.cls.Method[Unit, cls.Scp]]
  }

  val resultMessageCall = Variable[Any]

  val p1 = Variable[_root_.Simulation.RequestMessageInter[Unit]]
  val handleMessage = Foreach(
    code"$marketSelf.getRequestMessages",
    p1, LetBinding(
      Option(resultMessageCall),
      CallMethodC[Any](code"$p1.methodId", code"$p1.argss"),
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
              CallMethod[Unit](marketSell.sym, code"List(List(10))"),
              LetBinding(None,
                CallMethod[Unit](recursiveFunction.sym, code"List(List(List(10,20,30)))"),
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


  val tellP2:Variable[Int] = te.vparams.head.tail.head.asInstanceOf[Variable[Int]]
  val tell = new LiftedMethod[Unit](f, ScalaCode(code"$farmerSelf.happiness = $farmerSelf.happiness - ${tellP2}; ()"), true) {
    override val mtd: cls.Method[Unit, cls.Scp] = te.asInstanceOf[this.cls.Method[Unit, cls.Scp]]
  }

  val p2 = Variable[Farmer]
  val nofifyPeers = new LiftedMethod[Unit](
    f,
    Foreach[Farmer, Unit](
      code"$farmerSelf.peers",
      p2,
      Send[Unit](
        code"$farmerSelf",
        code"$p2",
        Message(tell, code"List(List($farmerSelf, $farmerSelf.happiness))")
      )
    ),
    false) {
    override val mtd: cls.Method[Unit, cls.Scp] = nP.asInstanceOf[this.cls.Method[Unit, cls.Scp]]
  }


  val farmer = ActorType[Farmer]("Farmer",
    State[Int](IR.methodSymbol[Farmer]("happiness"), code"0") ::
      State[List[Farmer]](IR.methodSymbol[Farmer]("peers"), code"Nil") :: Nil,
    tell :: nofifyPeers :: Nil,
    Forever(
      LetBinding(None,
        LetBinding[Int, Unit](Option(testResult), Send[Int](code"$farmerSelf", code"$farmerSelf.market", Message(marketSellB, code"List(List(500))")), ScalaCode(code"""println("TEST_VAR",$testResult)""")),
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
              CallMethod[Unit](marketSell.sym, code"List(List(1000))"),
              LetBinding(None,
                Wait(code"1"),
                LetBinding(None,
                  LetBinding(Option(testResult), ScalaCode[Int](code"42"), ScalaCode(code"println($testResult)")),
                  LetBinding(None,
                    LetBinding(Option(testResult), ScalaCode[Int](code"11"), ScalaCode(code"println($testResult)")),
                    LetBinding(Option(testResult), CallMethod[Int](marketSellB.sym, code"List(List(422))"), ScalaCode(code"println($testResult)"))
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
