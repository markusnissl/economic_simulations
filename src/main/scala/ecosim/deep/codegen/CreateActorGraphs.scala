package ecosim.deep.codegen

import ecosim.deep.algo.{Algo, AlgoInfo, NoOp, ScalaCode}
import ecosim.deep.member.ActorType
import squid.lib.MutVar

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import ecosim.deep.IR.Predef._

import scala.annotation.tailrec


//TODO ask markus if theres a problem with this
object CreateActorGraphs {
  case class MutVarType[A](variable: Variable[MutVar[A]], codeType: CodeType[A])
  val methodVariableTable: collection.mutable.Map[Int, ArrayBuffer[MutVarType[_]]] = collection.mutable.Map[Int, ArrayBuffer[MutVarType[_]]]()
  val methodVariableTableStack: collection.mutable.Map[Int, ArrayBuffer[Variable[ListBuffer[Any]]]] = collection.mutable.Map[Int, ArrayBuffer[Variable[ListBuffer[Any]]]]()
}

class CreateActorGraphs(actorTypes: List[ActorType[_]]) extends ConvertElement(actorTypes) {
  import CreateActorGraphs._

  override def run(): List[CompiledActorGraph] = {
    val graphs = actorTypes.map(createCompiledActorGraph)
    graphs.foreach(g => GraphDrawing.drawGraph(g.graph, g.name + "_original"))
    graphs
  }

  private val methodLookupTable: collection.mutable.Map[Int, Int] = collection.mutable.Map[Int, Int]()
  private val methodLookupTableEnd: collection.mutable.Map[Int, Int] = collection.mutable.Map[Int, Int]()
  private var variables: List[VarValue[_]] = List()

  /**
    * Inits the method variable lookup table
    *
    * @param data list of (methodId, list of parameter variables)
    */
  @tailrec
  private def createVariableTable(data: List[(Int, List[MutVarType[_]])]): Unit = data match {
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
  private def createCode(algo: Algo[_], isMethod: Boolean, methodId: Int = -1): Unit = {
    AlgoInfo.isMethod = isMethod

    //Method body is empty
    if (algo.isInstanceOf[NoOp[_]]) {
      ScalaCode(code"()").codegen
    } else {
      algo.codegen
    }
    AlgoInfo.nextPos()
  }

  /**
    * This code generates the complete code fragments of the actor
    *
    * @return a list of code, containing all code fragments of main code and method codes + initialized variable tables
    */
  def createCompiledActorGraph(actorType: ActorType[_]): CompiledActorGraph = {
    AlgoInfo.resetData()
    this.variables = List()

    // Generate code for main
    val main = createCode(actorType.main.asInstanceOf[Algo[Any]], false)

    // Generate code for methods
    val methodData = actorType.methods.map({
      case method => {
        methodLookupTable(method.methodId) = AlgoInfo.posCounter
        createCode(method.body.asInstanceOf[Algo[Any]], true, method.methodId)
        methodLookupTableEnd(method.methodId) = AlgoInfo.posCounter - 1


        val varList = ListBuffer[MutVarType[_]]()

        method.mtd.vparams.foreach(paramList => {
          paramList.foreach({case param:Variable[v] => {
            //To be honest, dont know why this is working now (Type)
            def ttt[Z](variable: Variable[Z]): Unit = {
              val cT = variable.Typ
              val methodArgsMut:Variable[MutVar[variable.Typ]] = Variable[MutVar[variable.Typ]];
              AlgoInfo.variables = AlgoInfo.VarWrapper[Z](variable, methodArgsMut) :: AlgoInfo.variables
              varList.append(MutVarType(methodArgsMut,cT))
            }
            ttt(param)

          }})
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
          val variable: MutVarType[_] = methodVariableTable(a)(b)

          variable match {
            case v:MutVarType[a] => {
              code"${v.variable} := $c.asInstanceOf[${v.codeType}]"
            }
            case _ => throw new RuntimeException("Illegal state")
          }
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
            val variable: MutVarType[_] = methodVariableTable(a)(c._2)
            val ab = c._1
            variable match {
              case v:MutVarType[a] => {
                code"$ab.remove(0); if(!$ab.isEmpty) {${v.variable} := $ab(0).asInstanceOf[${v.codeType}]}; $b; ()"
              }
              case _ => throw new RuntimeException("Illegal state")
            }
          })
        }
      })
    })

    variables = VarValue(AlgoInfo.returnValue,code"MutVar[Any](null)") :: VarValue(AlgoInfo.positionStack,code"ListBuffer[Int]()") :: VarValue(AlgoInfo.responseMessage,code"MutVar[ecosim.deep.member.ResponseMessage](null)") :: variables

    CompiledActorGraph(actorType.name, AlgoInfo.stateGraph.clone(), AlgoInfo.variables, variables, List[ActorType[_]](actorType), List[Variable[ListBuffer[Int]]](AlgoInfo.positionStack), AlgoInfo.posCounter)
  }


}
