package ecosim.example

import ecosim.deep.IR
import ecosim.deep.IR.TopLevel._
import ecosim.deep.IR.Predef._
import ecosim.deep.algo.AlgoInfo.EdgeInfo
import ecosim.deep.algo.{Algo, AlgoInfo, CallMethod, Foreach, Forever, IfThenElse, LetBinding, NoOp, ScalaCode, Send, Wait}
import ecosim.deep.codegen.{ClassCreation, GraphDrawing, InitCreation, MergeActors}
import ecosim.deep.member.{ActorType, LiftedMethod, RequestMessage, State}
import simulation.example.{Farmer, Market}

import scala.collection.mutable.ArrayBuffer

object CodegenExample extends App {

  def marketLifted(): ActorType[Market] = {
    val m: ClassWithObject[Market] = Market.reflect(IR)

    val mS = m.methods.find(_.symbol.asMethodSymbol.name.toString == "sell").get
    val mSB = m.methods.find(_.symbol.asMethodSymbol.name.toString == "sell2").get
    val rF = m.methods.find(_.symbol.asMethodSymbol.name.toString == "recursiveTest").get

    val marketSell = new LiftedMethod[Unit](m, ScalaCode(code"""println("Market sells: " + ${mS.vparams.head.head})"""), false, 1) {
      override val mtd: cls.Method[Unit, cls.Scp] = mS.asInstanceOf[this.cls.Method[Unit, cls.Scp]]
    }
    val marketSellB = new LiftedMethod[Int](m, ScalaCode[Int](code"""println("Market sells: " + ${mSB.vparams.head.head}); 42"""), true, 2) {
      override val mtd: cls.Method[Int, cls.Scp] = mSB.asInstanceOf[this.cls.Method[Int, cls.Scp]]
    }
    val marketSelf = Variable[Market]

    val rFParam1: Variable[_] = rF.vparams.head.head
    val recursiveFunction = new LiftedMethod[Unit](m, LetBinding(
      None,
      IfThenElse(code"${rFParam1}.asInstanceOf[List[Int]].tail.isEmpty == false", CallMethod[Unit](3, List(List(code"${rFParam1}.asInstanceOf[List[Int]].tail"))), NoOp[Unit]()),
      ScalaCode(code"""println(${rFParam1}.asInstanceOf[List[Int]].head);""")
    ), false, 3) {
      override val mtd: cls.Method[Unit, cls.Scp] = rF.asInstanceOf[this.cls.Method[Unit, cls.Scp]]
    }

    val resultMessageCall = Variable[Any]

    val p1 = Variable[RequestMessage]
    //Handle later

    val marketFunctions = marketSell :: marketSellB :: recursiveFunction :: Nil

    val algo: Algo[Any] = NoOp()
    val callCode = marketFunctions.zipWithIndex.foldRight(algo)((a, b) => {
      val argss: List[List[OpenCode[_]]] = a._1.mtd.vparams.zipWithIndex.map(x => {
        x._1.zipWithIndex.map(y => {
          code"$p1.argss(${Const(x._2)})(${Const(y._2)})"
        })
      })
      IfThenElse(code"$p1.methodId==${Const(a._2 + 1)}", CallMethod[Any](a._2 + 1, argss), b)
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
      State[List[String]](IR.methodSymbol[Market]("goods"), codeTypeOf[List[String]], code"Nil") :: Nil,
      marketFunctions,
      LetBinding(Option(bindingTest), ScalaCode[Int](code"0"),
        Forever(
          LetBinding(None, handleMessage,
            LetBinding(None,
              LetBinding(Option(bindingTest), ScalaCode[Int](code"$bindingTest + 1"), ScalaCode(code"""println("Binding test:",$bindingTest)""")),
              LetBinding(None,
                CallMethod[Unit](marketSell.methodId, List(List(code"10"))),
                LetBinding(None,
                  CallMethod[Unit](recursiveFunction.methodId, List(List(code"List(10,20,30)"))),
                  Wait(code"1")
                )
              )
            )
          )
        )
      ),
      marketSelf)

    market
  }

  def farmerLifted(marketSellB: LiftedMethod[_]): ActorType[Farmer] = {
    val f: ClassWithObject[Farmer] = Farmer.reflect(IR)
    val te = f.methods.find(_.symbol.asMethodSymbol.name.toString == "tell").get
    val nP = f.methods.find(_.symbol.asMethodSymbol.name.toString == "notifyPeers").get

    val farmerSelf = Variable[Farmer]
    val testResult = Variable[Int]


    val tellP2: Variable[Int] = te.vparams.head.tail.head.asInstanceOf[Variable[Int]]
    val tell = new LiftedMethod[Unit](f, ScalaCode(code"$farmerSelf.happiness = $farmerSelf.happiness - ${tellP2}; ()"), true, 1) {
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
          tell.methodId,
          List(List(code"$farmerSelf", code"$farmerSelf.happiness")),
          false
        )
      ),
      false, 2) {
      override val mtd: cls.Method[Unit, cls.Scp] = nP.asInstanceOf[this.cls.Method[Unit, cls.Scp]]
    }


    val farmer = ActorType[Farmer]("Farmer",
      State[Market](IR.methodSymbol[Farmer]("market"), codeTypeOf[Market], nullValue[Market].asOpenCode) ::
        State[Int](IR.methodSymbol[Farmer]("happiness"), codeTypeOf[Int], code"0") ::
        State[List[Farmer]](IR.methodSymbol[Farmer]("peers"), codeTypeOf[List[Farmer]], code"Nil") :: Nil,
      tell :: nofifyPeers :: Nil,
      Forever(
        LetBinding(None,
          LetBinding[Int, Unit](Option(testResult),
            Send[Int](
              code"$farmerSelf",
              code"$farmerSelf.market",
              marketSellB.methodId,
              List(List(code"500")),
              true
            ),
            ScalaCode(code"""println("TEST_VAR",$testResult)""")
          ),
          Wait(code"1")
        )
      )
      ,
      farmerSelf
    )
    farmer
  }


  val marketActorType = marketLifted()
  val farmerActorType = farmerLifted(marketActorType.methods.find(_.sym.asMethodSymbol.name.toString == "sell2").get)

  val actorTypes: List[ActorType[_]] = marketActorType :: farmerActorType :: Nil

  var graphs: Map[String, ArrayBuffer[EdgeInfo]] = Map()

  actorTypes.foreach({
    case x => {
      val cc = new ClassCreation(x, actorTypes)
      cc.run()
      graphs = graphs + (x.name -> AlgoInfo.stateGraph.clone())
    }
  })

  val wGM = MergeActors.waitGraph(graphs("Market"))
  GraphDrawing.drawGraph(wGM,"market_waitGraph")

  val wGF = MergeActors.waitGraph(graphs("Farmer"))
  GraphDrawing.drawGraph(wGF,"farmer_waitGraph")

  val mGMF = MergeActors.generateMergedStateMachine(wGM, wGF)
  GraphDrawing.drawGraph(mGMF,"farmer_market_mergedGraph")

  val ic = new InitCreation(code"""val m = new Market; val f = new Farmer(); f.market = m; List(m, f)""", actorTypes)
  ic.run()

}
