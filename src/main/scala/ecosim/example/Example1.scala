package ecosim.example.ex1

import ecosim.runtime._
import ecosim.deep.{ActorType, IR}
import IR.TopLevel._
import ecosim.classLifting.{MethodInfo, NBUnit}
import ecosim.example.{Farmer, Market}
import squid.quasi.lift
import simulation.{Message, RequestMessageInter}


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

  val p1 = Variable[ecosim.runtime.RequestMessage]
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
    code"$marketSelf.popRequestMessages",
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
      SendDebug[Unit](
        code"$farmerSelf",
        code"$p2",
        MessageDebug(tell.sym, List(List(code"$farmerSelf", code"$farmerSelf.happiness")))
      )
    ),
    false) {
    override val mtd: cls.Method[Unit, cls.Scp] = nP.asInstanceOf[this.cls.Method[Unit, cls.Scp]]
  }


  val farmer = ActorType[Farmer]("Farmer",
    State[Market](IR.methodSymbol[Farmer]("market"), nullValue[Market].asOpenCode) ::
    State[Int](IR.methodSymbol[Farmer]("happiness"), code"0") ::
      State[List[Farmer]](IR.methodSymbol[Farmer]("peers"), code"Nil") :: Nil,
    tell :: nofifyPeers :: Nil,
    Forever(
      LetBinding(None,
        LetBinding[Int, Unit](Option(testResult), SendDebug[Int](code"$farmerSelf", code"$farmerSelf.market", MessageDebug(marketSellB.sym, List(List(code"500")))), ScalaCode(code"""println("TEST_VAR",$testResult)""")),
        Wait(code"1")
      )
    ),
    farmerSelf)

  val actorTypes: List[ActorType[_]] = market :: farmer :: Nil

  var methodIdMapping: Map[IR.MtdSymbol, Int] = Map()
  var methodMapping: Map[Int, ecosim.classLifting.MethodInfo[_]] = Map()

  for (a <- actorTypes) {
    var counter = 0
    for (m <- a.methods) {
      methodIdMapping = methodIdMapping + (m.sym -> counter)
      var blocking = true
      if (m.mtd.A <:< codeTypeOf[NBUnit]) blocking = false
      methodMapping = methodMapping + (counter -> new MethodInfo[m.mtd.A](m.sym, m.mtd.tparams, m.mtd.vparams, blocking))
      counter = counter + 1
    }
  }

  val simulation = Simulation(actorTypes, code"val m = new Market; val f = new Farmer(); f.market = m; List(m, f)", methodIdMapping, methodMapping)

  //val actors = simulation.compile()

  /*val simu = new _root_.Simulation.Simulation()
  simu.init(actors)
  simu.run(7)*/


  val actors = simulation.codegen()
}
