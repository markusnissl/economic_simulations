package ecosim.deep.codegen

import java.io.File

import ecosim.deep.IR.Predef._
import ecosim.deep.algo.AlgoInfo.{CodeNodeMtd, CodeNodePos, EdgeInfo}
import ecosim.deep.algo.{Algo, AlgoInfo}
import ecosim.deep.member.ActorType
import guru.nidi.graphviz.attribute.{Color, Label, RankDir, Style}
import guru.nidi.graphviz.engine.{Format, Graphviz}
import guru.nidi.graphviz.model.{Factory, Graph}
import squid.lib.MutVar

import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.runtime.ScalaRunTime

abstract class Codegen(actorType: ActorType[_]) {

  /**
    * Generate the stepwise code for the given algo
    *
    * @param algo     contains the code, which should be generated
    * @param isMethod if true, then a restore position is added at the end
    * @return a list of code steps
    */
  private def createCode(algo: Algo[_], isMethod: Boolean): List[OpenCode[Unit]] = {
    AlgoInfo.isMethod = isMethod
    var commands = algo.codegen
    if (isMethod) {
      commands = commands ::: List(AlgoInfo.restorePosition)
      // The state is handled in the callMethod part, where the destination is known, but pos has to be increased
      AlgoInfo.nextPos
    }

    commands
  }

  /**
    * Converts a list of opencode code fragments to a opencode of list of code fragments
    */
  def createCommandOpenCode(commands: List[OpenCode[Unit]]): OpenCode[List[() => Unit]] = {
    val start: OpenCode[List[() => Unit]] = code"List[() => Unit]()"
    commands.map(x => code"List(() => ${x})").foldLeft(start)((x, y) => code"$x ::: $y")
  }

  val methodLookupTable: collection.mutable.Map[Int, Int] = collection.mutable.Map[Int, Int]()
  val methodLookupTableEnd: collection.mutable.Map[Int, Int] = collection.mutable.Map[Int, Int]()
  var methodVariableTable: collection.mutable.Map[Int, ArrayBuffer[Variable[MutVar[Any]]]] = collection.mutable.Map[Int, ArrayBuffer[Variable[MutVar[Any]]]]()
  var methodVariableTableStack: collection.mutable.Map[Int, ArrayBuffer[Variable[ListBuffer[Any]]]] = collection.mutable.Map[Int, ArrayBuffer[Variable[ListBuffer[Any]]]]()

  /**
    * Inits the methodLookupTable with the correct positions of the code
    *
    * @param methodData a list of tuple (methodId, length)
    * @param currentPos current position to start filling the table with the first entry in the list
    */
  @tailrec
  private def createMethodTable(methodData: List[(Int, Int)], currentPos: Int): Unit = methodData match {
    case (x :: xs) =>
      methodLookupTable(x._1) = currentPos
      methodLookupTableEnd(x._1) = currentPos + x._2 - 1 //Last pos thus -1
      createMethodTable(xs, currentPos + x._2)
    case Nil => ()
  }

  /**
    * Inits the method variable lookup table
    *
    * @param data list of (methodId, list of parameter variables)
    */
  @tailrec
  private def createVariableTable(data: List[(Int, List[Variable[MutVar[Any]]])]): Unit = data match {
    case (x :: xs) => {
      methodVariableTable(x._1) = ArrayBuffer(x._2: _*)
      createVariableTable(xs)
    }
    case Nil => ()
  }

  /**
    * This class is used to create the variable stack for each parameter variable of a method
    *
    * @param variable to be used in code
    * @param init     code to init variable
    * @param A        type of variable
    * @tparam C variable type
    */
  case class VarValue[C](variable: Variable[C], init: OpenCode[C])(implicit val A: CodeType[C])

  var variables: List[VarValue[_]] = List()

  /**
    * Inits the stack for the method variable storage
    *
    * @param data foramt List[(methodId, amount of variables)]
    */
  @tailrec
  private def createVariableTableStack(data: List[(Int, Int)]): Unit = data match {
    case (x :: xs) => {
      val a = ArrayBuffer[Variable[ListBuffer[Any]]]()
      for (i <- 0 until x._2) {
        val x = Variable[ListBuffer[Any]]
        a.append(x)
        variables = VarValue(x, code"ListBuffer[Any]()") :: variables
      }
      methodVariableTableStack(x._1) = a
      createVariableTableStack(xs)
    }
    case Nil => ()
  }

  /**
    * Generates init code of one variable of type VarWrapper
    *
    * @param variable of type VarWrapper
    * @param rest     Code, where variables should be applied to
    * @tparam A type of variable
    * @tparam R return type of code
    * @return rest with bounded variable
    */
  private def initVar[A, R: CodeType](variable: AlgoInfo.VarWrapper[A], rest: OpenCode[R]): OpenCode[R] = {
    import variable.A
    code"val ${variable.to} = MutVar(${nullValue[A]}); $rest"
  }

  /**
    * Generates init code for variables of a list of VarWrappers
    *
    * @param variables list of varWrapper
    * @param after     code, where variables should be applied to
    * @tparam R return type of code
    * @return after with bounded variables
    */
  def generateMutVarInit[R: CodeType](variables: List[AlgoInfo.VarWrapper[_]], after: OpenCode[R]): OpenCode[R] = variables match {
    case Nil => code"$after"
    case (x :: xs) => {
      initVar(x, generateMutVarInit(xs, after))
    }
  }

  /**
    * Generates init code of one variable of type VarValue
    *
    * @param variable variable of type VarValue
    * @param rest     Code, where variables should be applied to
    * @tparam A type of variable
    * @tparam R return type of code
    * @return rest with bounded variable
    */
  private def initVar2[A, R: CodeType](variable: VarValue[A], rest: OpenCode[R]): OpenCode[R] = {
    code"val ${variable.variable} = ${variable.init}; $rest"
  }

  /**
    * generates init code for variables of list of type VarValue
    *
    * @param variables list of varvalue
    * @param after     code, where variables should be applied to
    * @tparam R return type of code
    * @return after with bounded variables
    */
  def generateVarInit[R: CodeType](variables: List[VarValue[_]], after: OpenCode[R]): OpenCode[R] = variables match {
    case Nil => code"$after"
    case (x :: xs) => {
      initVar2(x, generateVarInit(xs, after))
    }
  }

  /**
    * This code generates the complete code fragments of the actor
    *
    * @return a list of code, containing all code fragments of main code and method codes + initialized variable tables
    */
  def createLists(): List[OpenCode[Unit]] = {
    AlgoInfo.stateGraph.clear()
    AlgoInfo.posCounter = 0

    // Generate code for main
    val main = createCode(actorType.main.asInstanceOf[Algo[Any]], false)

    // Generate code for methods
    val methodData = actorType.methods.map({
      case method => {
        var commands = createCode(method.body.asInstanceOf[Algo[Any]], true)
        var varList = ListBuffer[Variable[MutVar[Any]]]()
        method.mtd.vparams.foreach(paramList => {
          paramList.foreach(param => {
            val methodArgsMut = Variable[MutVar[Any]];
            varList.append(methodArgsMut)
            AlgoInfo.variables = AlgoInfo.VarWrapper(param.asInstanceOf[Variable[Any]], methodArgsMut) :: AlgoInfo.variables
            val input = param.asInstanceOf[Variable[Any]]
            val typ = param.Typ
            commands = commands.map(x => x.subs(input).~>(code"($methodArgsMut!).asInstanceOf[$typ]"))
          })
        })
        (commands, varList, method.methodId)
      }
    })

    // Fill the tables
    createMethodTable(methodData.map(x => (x._3, x._1.length)), main.length)
    createVariableTable(methodData.map(x => (x._3, x._2.toList)))
    createVariableTableStack(methodData.map(x => (x._3, x._2.length)))

    // Merge code to a big list
    val mergedCommands = methodData.foldLeft(main)((a, b) => (a ::: b._1))

    AlgoInfo.convertStageGraph(methodLookupTable.toMap, methodLookupTableEnd.toMap)

    optimizeCode(mergedCommands.length)
    drawGraph()




    // Replace special code instructions, with real data, after this data has been calculated
    val replacedCommands = mergedCommands.map(x => {
      x.rewrite({
        case code"ecosim.deep.algo.Instructions.getMethodPosition(${Const(a)})    " => Const(methodLookupTable(a))
        case code"ecosim.deep.algo.Instructions.setMethodParam(${Const(a)}, ${Const(b)}, $c)    " => {
          val variable: Variable[MutVar[Any]] = methodVariableTable(a)(b)
          code"$variable := $c"
        }
        case code"ecosim.deep.algo.Instructions.saveMethodParam(${Const(a)}, ${Const(b)}, $c)    " => {
          val stack: ArrayBuffer[Variable[ListBuffer[Any]]] = methodVariableTableStack(a)
          val varstack: Variable[ListBuffer[Any]] = stack(b)
          code"$varstack.prepend($c);"
        }
        case code"ecosim.deep.algo.Instructions.restoreMethodParams(${Const(a)})    " => {
          val stack: ArrayBuffer[Variable[ListBuffer[Any]]] = methodVariableTableStack(a)
          val initCode: OpenCode[Unit] = code"()"
          stack.zipWithIndex.foldRight(initCode)((c, b) => {
            val variable: Variable[MutVar[Any]] = methodVariableTable(a)(c._2)
            val ab = c._1
            code"$ab.remove(0); if(!$ab.isEmpty) {$variable := $ab(0)}; $b; ()"
          })
        }
      })
    })

    replacedCommands
  }

  def optimizeCode(commandsAmount: Int): Unit = {
    //Create matrix of incoming and outgoing edges
    //Each row contains outgoing edges to other
    //Each column contains incoming edges from others
    val m = Array.fill[Array[Int]](commandsAmount)(Array.fill[Int](commandsAmount)(0))
    AlgoInfo.stateGraph.foreach(x => {
      m(x.from.asInstanceOf[CodeNodePos].pos)(x.to.asInstanceOf[CodeNodePos].pos) = 1
    })

    val outgoing = m.map(_.sum)
    val incoming = m.transpose.map(_.sum)


    println(m.map(_.mkString(",")).mkString("\n"))

    println("---")
    println(outgoing.mkString(","))
    println(incoming.mkString(","))
  }


  def drawGraph(): Unit = {
    var g: Graph = Factory.graph("ExecutionGraph")
      .directed().graphAttr()
      .`with`(RankDir.LEFT_TO_RIGHT)

    AlgoInfo.stateGraph.groupBy(_.from.getId).foreach(y => {
      // It is required to create each node only once, otherwise some links get lost
      var nStart = Factory.node(y._1)
      y._2.foreach(x => {
        if (x.waitEdge) {
          nStart = nStart.link(Factory.to(Factory.node(x.to.getId)).`with`(Color.RED).`with`(Label.of(x.label)))
        } else if (x.isMethod) {
          nStart = nStart.link(Factory.to(Factory.node(x.to.getId)).`with`(Color.BLUE).`with`(Label.of(x.label)))
        } else {
          nStart = nStart.link(Factory.to(Factory.node(x.to.getId)).`with`(Color.BLACK).`with`(Label.of(x.label)))
        }
      })
      g = g.`with`(nStart)
    })


    val gviz = Graphviz.fromGraph(g)

    gviz.render(Format.PNG).toFile(new File("debug/graph_" + actorType.name + ".png"));
  }

  def run(): Unit

}
