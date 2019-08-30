package ecosim.deep.codegen

import ecosim.deep.algo.{Algo, AlgoInfo}
import ecosim.deep.member.ActorType
import squid.lib.MutVar

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import ecosim.deep.IR.Predef._

import scala.annotation.tailrec

class CreateActorGraphs(actorTypes: List[ActorType[_]]) extends ConvertElement(actorTypes) {

  override def run(): List[CompiledActorGraph] = {
    val graphs = actorTypes.map(createCompiledActorGraph)
    graphs.foreach(g => GraphDrawing.drawGraph(g.graph, g.name + "_original"))
    graphs
  }

  private val methodLookupTable: collection.mutable.Map[Int, Int] = collection.mutable.Map[Int, Int]()
  private val methodLookupTableEnd: collection.mutable.Map[Int, Int] = collection.mutable.Map[Int, Int]()
  private val methodVariableTable: collection.mutable.Map[Int, ArrayBuffer[Variable[MutVar[Any]]]] = collection.mutable.Map[Int, ArrayBuffer[Variable[MutVar[Any]]]]()
  private val methodVariableTableStack: collection.mutable.Map[Int, ArrayBuffer[Variable[ListBuffer[Any]]]] = collection.mutable.Map[Int, ArrayBuffer[Variable[ListBuffer[Any]]]]()
  private var variables: List[VarValue[_]] = List()

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
    * Generate the stepwise code for the given algo
    *
    * @param algo     contains the code, which should be generated
    * @param isMethod if true, then a restore position is added at the end
    * @return a list of code steps
    */
  private def createCode(algo: Algo[_], isMethod: Boolean): Unit = {
    AlgoInfo.isMethod = isMethod
    algo.codegen
    AlgoInfo.nextPos()
  }

  /**
    * This code generates the complete code fragments of the actor
    *
    * @return a list of code, containing all code fragments of main code and method codes + initialized variable tables
    */
  def createCompiledActorGraph(actorType: ActorType[_]): CompiledActorGraph = {
    AlgoInfo.stateGraph.clear()
    AlgoInfo.variables = List()
    AlgoInfo.varSavers = List()
    AlgoInfo.posCounter = 0

    // Generate code for main
    val main = createCode(actorType.main.asInstanceOf[Algo[Any]], false)

    // Generate code for methods
    val methodData = actorType.methods.map({
      case method => {
        methodLookupTable(method.methodId) = AlgoInfo.posCounter
        createCode(method.body.asInstanceOf[Algo[Any]], true)
        methodLookupTableEnd(method.methodId) = AlgoInfo.posCounter - 1


        val varList = ListBuffer[Variable[MutVar[Any]]]()
        method.mtd.vparams.foreach(paramList => {
          paramList.foreach(param => {
            val methodArgsMut = Variable[MutVar[Any]];
            varList.append(methodArgsMut)
            AlgoInfo.variables = AlgoInfo.VarWrapper(param.asInstanceOf[Variable[Any]], methodArgsMut) :: AlgoInfo.variables
          })
        })
        (varList, method.methodId)
      }
    })

    // Fill the tables
    createVariableTable(methodData.map(x => (x._2, x._1.toList)))
    createVariableTableStack(methodData.map(x => (x._2, x._1.length)))

    AlgoInfo.convertStageGraph(methodLookupTable.toMap, methodLookupTableEnd.toMap)

    AlgoInfo.stateGraph.foreach(edge => {
      edge.code = edge.code.rewrite({
        case code"ecosim.deep.algo.Instructions.setMethodParam(${Const(a)}, ${Const(b)}, $c)" => {
          val variable: Variable[MutVar[Any]] = methodVariableTable(a)(b)
          code"$variable := $c"
        }
        case code"ecosim.deep.algo.Instructions.saveMethodParam(${Const(a)}, ${Const(b)}, $c)" => {
          val stack: ArrayBuffer[Variable[ListBuffer[Any]]] = methodVariableTableStack(a)
          val varstack: Variable[ListBuffer[Any]] = stack(b)
          code"$varstack.prepend($c);"
        }
        case code"ecosim.deep.algo.Instructions.restoreMethodParams(${Const(a)})" => {
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

    CompiledActorGraph(actorType.name, AlgoInfo.stateGraph.clone(), AlgoInfo.variables, variables, List[ActorType[_]](actorType))
  }


}
