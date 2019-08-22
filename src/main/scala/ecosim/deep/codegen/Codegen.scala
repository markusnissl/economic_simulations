package ecosim.deep.codegen

import ecosim.deep.IR.Predef._
import ecosim.deep.algo.{Algo, AlgoInfo}
import ecosim.deep.member.ActorType
import squid.lib.MutVar

import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

abstract class Codegen(actorType: ActorType[_]) {

  private def createCode(algo: Algo[_], isMethod: Boolean): List[OpenCode[Unit]] = {
    AlgoInfo.merger.clear()
    var commands = algo.codegen
    if (isMethod) {
      commands = commands ::: List(AlgoInfo.restorePosition)
      AlgoInfo.merger.append((true, false))
    }

    AlgoInfo.mergeCodes(commands, AlgoInfo.merger.toList)
  }

  def createCommandOpenCode(commands: List[OpenCode[Unit]]): OpenCode[List[() => Unit]] = {
    val start: OpenCode[List[() => Unit]] = code"List[() => Unit]()"
    commands.map(x => code"List(() => ${x})").foldLeft(start)((x, y) => code"$x ::: $y")
  }

  val methodLookupTable: collection.mutable.Map[Int, Int] = collection.mutable.Map[Int, Int]()
  var methodVariableTable: collection.mutable.Map[Int, ArrayBuffer[Variable[MutVar[Any]]]] = collection.mutable.Map[Int, ArrayBuffer[Variable[MutVar[Any]]]]()
  var methodVariableTableStack: collection.mutable.Map[Int, ArrayBuffer[Variable[ListBuffer[Any]]]] = collection.mutable.Map[Int, ArrayBuffer[Variable[ListBuffer[Any]]]]()

  @tailrec
  private def createMethodTable(methodData: List[(Int, Int)], currentPos: Int): Unit = methodData match {
    case (x :: xs) =>
      methodLookupTable(x._1) = currentPos
      createMethodTable(xs, currentPos + x._2)
    case Nil => ()
  }

  @tailrec
  private def createVariableTable(data: List[(Int, List[Variable[MutVar[Any]]])]): Unit = data match {
    case (x :: xs) => {
      methodVariableTable(x._1) = ArrayBuffer(x._2: _*)
      createVariableTable(xs)
    }
    case Nil => ()
  }

  case class VarValue[C](val variable: Variable[C], val init: OpenCode[C])(implicit val A: CodeType[C])

  var variables2: List[VarValue[_]] = List()

  @tailrec
  private def createVariableTableStack(data: List[(Int, Int)]): Unit = data match {
    case (x :: xs) => {
      val a = ArrayBuffer[Variable[ListBuffer[Any]]]()
      for (i <- 0 until x._2) {
        val x = Variable[ListBuffer[Any]]
        a.append(x)
        variables2 = VarValue(x, code"ListBuffer[Any]()") :: variables2
      }
      methodVariableTableStack(x._1) = a
      createVariableTableStack(xs)
    }
    case Nil => ()
  }

  private def initVar[A, R: CodeType](variable: AlgoInfo.VarWrapper[A], rest: OpenCode[R]): OpenCode[R] = {
    import variable.A
    code"val ${variable.to} = MutVar(${nullValue[A]}); $rest"
  }

  def generateMutVarInit[R: CodeType](variables: List[AlgoInfo.VarWrapper[_]], after: OpenCode[R]): OpenCode[R] = variables match {
    case Nil => code"$after"
    case (x :: xs) => {
      initVar(x, generateMutVarInit(xs, after))
    }
  }

  private def initVar2[A, R: CodeType](variable: VarValue[A], rest: OpenCode[R]): OpenCode[R] = {
    import variable.A
    code"val ${variable.variable} = ${variable.init}; $rest"
  }

  def generateVarInit[R: CodeType](variables: List[VarValue[_]], after: OpenCode[R]): OpenCode[R] = variables match {
    case Nil => code"$after"
    case (x :: xs) => {
      initVar2(x, generateVarInit(xs, after))
    }
  }

  def createLists(): List[OpenCode[Unit]] = {
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

    val main = createCode(actorType.main.asInstanceOf[Algo[Any]], false)

    createMethodTable(methodData.map(x => (x._3, x._1.length)), main.length)
    createVariableTable(methodData.map(x => (x._3, x._2.toList)))
    createVariableTableStack(methodData.map(x => (x._3, x._2.length)))

    val mergedCommands = methodData.foldLeft(main)((a, b) => (a ::: b._1))

    val replacedCommands = mergedCommands.map(x => {
      x.rewrite({
        case code"ecosim.deep.algo.Instructions.getMethodPosition(${Const(a)}) " => Const(methodLookupTable(a))
        case code"ecosim.deep.algo.Instructions.setMethodParam(${Const(a)}, ${Const(b)}, $c) " => {
          val variable: Variable[MutVar[Any]] = methodVariableTable(a)(b)
          code"$variable := $c"
        }
        case code"ecosim.deep.algo.Instructions.saveMethodParam(${Const(a)}, ${Const(b)}, $c) " => {
          val stack: ArrayBuffer[Variable[ListBuffer[Any]]] = methodVariableTableStack(a)
          val varstack: Variable[ListBuffer[Any]] = stack(b)
          code"$varstack.prepend($c);"
        }
        case code"ecosim.deep.algo.Instructions.restoreMethodParams(${Const(a)}) " => {
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

  def run(): Unit

}
