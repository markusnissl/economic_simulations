package ecosim.example.ex1

import ecosim.runtime._
import ecosim.deep.{ActorType, IR}
import IR.TopLevel._
import squid.quasi.lift
import simulation.Message

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


/**
  * This is a demo environment for running the simulations.
  * The idea is to run this one on the driver, which creates and deletes the actors accordingly.
  * Wait for code compilation to see how it works :)
  */
@lift
class Environment() extends Actor {

  private var actorTypes: List[ActorType[Actor]] = List()
  private val market: Market = new Market()

  private var actorList: List[Actor] = List()
  private var messages: List[Message] = List()

  //TODO: replace this function by the actual method call while compiling
  def call(methodId: Int, argss: List[List[Any]]): Any = {
    ???
  }

  def handleMessagesNew(): Unit = {
    this.getRequestMessages.foreach(x => {
      x.reply(this, call(x.methodId, x.argss))
    })
  }

  def main(): Unit = {
    createFarmer()

    //Local mode
    while (true) {
      val mx = messages.groupBy(_.receiverId)
      this.setReceiveMessages(mx.getOrElse(this.id, List()))

      handleMessagesNew()

      for(el <- actorList) {
        el.setReceiveMessages(mx.getOrElse(el.id, List()))
        el.run_until(current_time)
      }
      messages = actorList.flatMap(_.getMessages)
    }
  }

  def getMarket(): Market = {
    market
  }

  def createFarmer(): Farmer = {
    val aT: ActorType[Farmer] = this.actorTypes.find(_.name == "Farmer").get.asInstanceOf[ActorType[Farmer]]
    val farmer = new Farmer(market)
    farmer.useStepFunction = true
    farmer.stepFunction = aT.getStepFunction(farmer)
    actorList = farmer :: actorList
    farmer
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

  val rFParam1: Variable[_] = rF.vparams.head.head
  val recursiveFunction = new LiftedMethod[Unit](m, LetBinding(
    None,
    If(code"${rFParam1}.asInstanceOf[List[Int]].tail.isEmpty == false", CallMethodDebug[Unit](rF.symbol, List(List(code"${rFParam1}.asInstanceOf[List[Int]].tail")))),
    ScalaCode(code"""println(${rFParam1}.asInstanceOf[List[Int]].head);""")
  ), false) {
    override val mtd: cls.Method[Unit, cls.Scp] = rF.asInstanceOf[this.cls.Method[Unit, cls.Scp]]
  }

  val resultMessageCall = Variable[Any]

  val p1 = Variable[_root_.simulation.RequestMessageInter]
  //Handle later

  val marketFunctions = marketSell :: marketSellB :: recursiveFunction :: Nil

  val algo: Algo[Any] = NoOp()
  val callCode = marketFunctions.zipWithIndex.foldRight(algo)((a, b) => {
    val argss: List[List[OpenCode[_]]] = a._1.mtd.vparams.zipWithIndex.map(x => {
      x._1.zipWithIndex.map(y => {
        code"$p1.argss(${Const(x._2)})(${Const(y._2)})"
      })
    })
    IfElse(code"$p1.methodId==${Const(a._2)}", CallMethod[Any](a._2, argss), b)
  })

  val handleMessage = Foreach(
    code"$marketSelf.getRequestMessages",
    p1, LetBinding(
      Option(resultMessageCall),
      callCode,
      ScalaCode(code"""$p1.reply($marketSelf, $resultMessageCall)""")
    )
  )


  val bindingTest = Variable[Int]

  val market = ActorType[Market]("Market",
    State[List[String]](IR.methodSymbol[Market]("goods"), code"Nil") :: Nil,
    marketFunctions,
    LetBinding(Option(bindingTest), ScalaCode[Int](code"0"),
      Forever(
        LetBinding(None, handleMessage,
          LetBinding(None,
            LetBinding(Option(bindingTest), ScalaCode[Int](code"$bindingTest + 1"), ScalaCode(code"""println("Binding test:",$bindingTest)""")),
            LetBinding(None,
              CallMethodDebug[Unit](marketSell.sym, List(List(code"10"))),
              LetBinding(None,
                CallMethodDebug[Unit](recursiveFunction.sym, List(List(code"List(10,20,30)"))),
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


  val tellP2: Variable[Int] = te.vparams.head.tail.head.asInstanceOf[Variable[Int]]
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
        Message(tell, List(List(code"$farmerSelf", code"$farmerSelf.happiness")))
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
        LetBinding[Int, Unit](Option(testResult), Send[Int](code"$farmerSelf", code"$farmerSelf.market", Message(marketSellB, List(List(code"500")))), ScalaCode(code"""println("TEST_VAR",$testResult)""")),
        Wait(code"1")
      )
    ),
    farmerSelf)

  val actorTypes: List[ActorType[_]] = market :: farmer :: Nil

  var methodIdMapping: Map[IR.MtdSymbol, Int] = Map()


  for (a <- actorTypes) {
    var counter = 0
    for (m <- a.methods) {
      methodIdMapping = methodIdMapping + (m.sym -> counter)
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
              CallMethodDebug[Unit](marketSell.sym, List(List(code"1000"))),
              LetBinding(None,
                Wait(code"1"),
                LetBinding(None,
                  LetBinding(Option(testResult), ScalaCode[Int](code"42"), ScalaCode(code"println($testResult)")),
                  LetBinding(None,
                    LetBinding(Option(testResult), ScalaCode[Int](code"11"), ScalaCode(code"println($testResult)")),
                    LetBinding(Option(testResult), CallMethodDebug[Int](marketSellB.sym, List(List(code"422"))), ScalaCode(code"println($testResult)"))
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
  val simu = new old.Simulation()
  simu.init(actors)
  simu.run(7)
}
