package ecosim.deep

import IR.Predef._
import squid.lib.MutVar
import _root_.Simulation.{RequestMessageInter, ResponseMessageInter}
import code.__wait

import scala.collection.mutable.ListBuffer

class Codegen[X <: ecosim.runtime.Actor](methodIdMapping: Map[Int, IR.MtdSymbol], actorType: ActorType[X])(implicit val X: CodeType[X]) {

  case class VarWrapper[C](val from: Variable[C], val to: Variable[MutVar[C]])(implicit val A: CodeType[C])


  val timer = Variable[MutVar[Int]]
  val pos = Variable[MutVar[Int]]

  val posSafer = Variable[MutVar[List[Int]]]
  val returnValue = Variable[MutVar[Any]]

  val responseMessage = Variable[MutVar[ResponseMessageInter[Any]]]

  var methodLookupTableVar = Variable[MutVar[Map[Int, Int]]]
  var methodLookupTable: OpenCode[Map[Int, Int]] = code"Map[Int, Int]()"
  var methodVariableTable: OpenCode[Map[Int, MutVar[List[MutVar[Any]]]]] = code"Map[Int, MutVar[List[MutVar[Any]]]]()"
  var methodVariableTableVar = Variable[MutVar[Map[Int, MutVar[List[MutVar[Any]]]]]]
  var methodVariableTableStack: OpenCode[Map[Int, MutVar[List[List[Any]]]]] = code"Map[Int, MutVar[List[List[Any]]]]()"
  var methodVariableTableVarStack = Variable[MutVar[Map[Int, MutVar[List[List[Any]]]]]]

  var variables: List[VarWrapper[_]] = List()

  var varSavers = List[VarWrapper[_]]()

  var merger: ListBuffer[(Boolean, Boolean)] = ListBuffer()

  def initVar[A, R: CodeType](variable: VarWrapper[A], rest: OpenCode[R]): OpenCode[R] = {
    import variable.A
    code"val ${variable.to} = MutVar(${nullValue[A]}); $rest"
  }

  def generateMutVarInit[R: CodeType](variables: List[VarWrapper[_]], after: OpenCode[R]): OpenCode[R] = variables match {
    case Nil => code"$after"
    case (x :: xs) => {
      initVar(x, generateMutVarInit(xs, after))

    }
  }

  def generateMethodTable(data: List[(OpenCode[List[() => Unit]], List[Variable[_]], Int)], pos: OpenCode[Int]): OpenCode[Map[Int, Int]] = data match {
    case (x :: xs) => code"""val l = ${x._1}.length; $${generateMethodTable(xs, code"$$pos + l")} + (${Const(x._3)} -> $pos)"""
    case Nil => code"Map[Int, Int]()"
  }

  def getVariableTablelist(data: List[Variable[MutVar[Any]]]): OpenCode[List[MutVar[Any]]] = data match {
    case x :: xs => code"$x :: ${getVariableTablelist(xs)}"
    case Nil => code"Nil"
  }

  def generateVariableTable(data: List[(OpenCode[List[() => Unit]], List[Variable[MutVar[Any]]], Int)]): OpenCode[Map[Int, MutVar[List[MutVar[Any]]]]] = data match {
    case (x :: xs) => code"""Map(${Const(x._3)} -> MutVar(${getVariableTablelist(x._2)})) ++ $${generateVariableTable(xs)}"""
    case Nil => code"Map[Int, MutVar[List[MutVar[Any]]]]()"
  }

  def generateVariableTableStack(data: List[(OpenCode[List[() => Unit]], List[Variable[MutVar[Any]]], Int)]): OpenCode[Map[Int, MutVar[List[List[Any]]]]] = data match {
    case (x :: xs) => {
      val entry = code"Map(${Const(x._3)} -> MutVar(List[List[Any]]()))"
      code"""$entry ++ $${generateVariableTableStack(xs)}"""
    }
    case Nil => code"Map[Int, MutVar[List[List[Any]]]]()"
  }

  //This is not supported :(
  //var testMethMap:ClosedCode[Map[Int, Variable[MutVar[_]]]] = code"Map[Int,Variable[MutVar[_]]]()"

  private def codeGenMethod(method: LiftedMethod[_]): (OpenCode[List[() => Unit]], List[Variable[MutVar[Any]]], Int) = {

    //FIXME: Cannot create a map in squid with _ param, therefore have to use any
    // Cannot define map outside, since methods may have recursive dependency, so lookup has to be made inside squid

    var code = this.createCode(method.body, true)

    var varList = ListBuffer[Variable[MutVar[Any]]]()

    method.mtd.vparams.foreach(paramList => {
      paramList.foreach(param => {
        val methodArgsMut = Variable[MutVar[Any]];
        varList.append(methodArgsMut)
        variables = VarWrapper(param.asInstanceOf[Variable[Any]], methodArgsMut) :: variables
        val tmp = code
        val input = param.asInstanceOf[Variable[Any]]
        val typ = param.Typ
        code = tmp.subs(input).~>(code"($methodArgsMut!).asInstanceOf[$typ]")
      }
      )
    })

    //    val methodArgsMut = Variable[MutVar[A]];

    //variables = VarWrapper(methodArgs, methodArgsMut) :: variables
    (code, varList.toList, methodIdMapping.map(_.swap).get(method.sym).get)
  }


  def compile(selfRef: X): (Int, Int, Int) => (Int, Int) = {

    val methodCodes = this.actorType.methods.map(mtd => codeGenMethod(mtd))
    val main = this.createCode(this.actorType.main)
    val commands = methodCodes.foldLeft(main)((x, y) => code"$x ::: ${y._1}")

    methodLookupTable = generateMethodTable(methodCodes, code"$main.length")
    methodVariableTable = generateVariableTable(methodCodes)
    methodVariableTableStack = generateVariableTableStack(methodCodes)

    val finalCode = this.createExec(this.actorType.self, commands)

    val f = finalCode.compile
    f(selfRef)

  }


  def createExec(selfVar: Variable[X], commands: OpenCode[List[() => Unit]]): ClosedCode[(X) => (Int, Int, Int) => (Int, Int)] = {
    generateMutVarInit(variables,
      code"""(self:X) => {
         val $returnValue = MutVar(null)
         val $posSafer = MutVar(List())
         val $selfVar = self
         val $responseMessage = MutVar(null)
         val $timer = MutVar(0)
         val $pos = MutVar(0)
         val $methodLookupTableVar = MutVar(Map())
         val $methodVariableTableVar = MutVar(Map())
         val $methodVariableTableVarStack = MutVar(Map())

         $methodLookupTableVar := $methodLookupTable
         $methodVariableTableVar := $methodVariableTable
         $methodVariableTableVarStack := $methodVariableTableStack

         val commandLength = $commands.length

          (startPos: Int, startTime: Int, endTime: Int) => {
            $timer := startTime
            $pos := startPos

            while (($timer!) <= endTime && ($pos!) < commandLength) {
              val command = $commands(($pos!))
              command()
              $pos := ($pos!) + 1
            }

            (($pos!),($timer!))
          }
        }
      """).unsafe_asClosedCode
  }

  def createCode(algo: Algo[_], isMethod: Boolean = false): OpenCode[List[() => Unit]] = {
    merger.clear()
    var commands: List[OpenCode[Unit]] = createCodeLogic(algo)
    if (isMethod) {
      val f1 = code"$pos := ((($posSafer!).head) - 1); $posSafer := ($posSafer!).tail; ()"
      commands = commands ::: List(f1)
      merger.append((true, false))
    }

    commands = mergeCodes(commands, merger.toList)

    println(commands)

    val start: OpenCode[List[() => Unit]] = code"List[() => Unit]()"
    commands.map(x => code"List(() => ${x})").foldLeft(start)((x, y) => code"$x ::: $y")
  }

  private def mergeCodes(com: List[OpenCode[Unit]], mergeInfo: List[(Boolean, Boolean)]): List[OpenCode[Unit]] = {
    if (mergeInfo.isEmpty) {
      com
    } else if (mergeInfo.tail.isEmpty) {
      com
    } else {
      val current = mergeInfo.head
      val next = mergeInfo.tail.head

      if (current._2 && next._1) {
        val newMergeInfo = (current._1, next._2) :: mergeInfo.tail.tail
        val a = com.head
        val b = com.tail.head
        val comNew = (code"$a; $b") :: com.tail.tail
        mergeCodes(comNew, newMergeInfo)
      } else {
        com.head :: mergeCodes(com.tail, mergeInfo.tail)
      }
    }
  }

  /**
    * Note:
    * Not allowed to set true or false to beginning or ending, otherwise if jumps would be wrong and not working
    */
  private def mergeMerger(com: List[OpenCode[Unit]]): Unit = {
    com.foreach(x => merger.append((false, false)))
  }

  private def createCodeLogic(algo: Algo[_]): List[OpenCode[Unit]] = {
    algo match {
      case Forever(bdy) => {
        val listVar = Variable[MutVar[List[Any]]]

        val f1: OpenCode[Unit] = code"""$posSafer := ($pos!) :: ($posSafer!); ()"""
        val a = code"""List[() => Unit](() => $f1)"""
        merger.append((false, true))

        val x = createCodeLogic(bdy)

        val f2: OpenCode[Unit] = code"""$pos := ((($posSafer!).head) - 1); $posSafer := ($posSafer!).tail; ()"""
        merger.append((true, false))

        val e = code"""List[() => Unit](() => $f2)"""


        List(f1) ::: x ::: List(f2)
      }
      case sc: ScalaCode[a] => {
        import sc.tpe

        val tmp = code"${sc.cde}"
        val met: OpenCode[Unit] = code"""$returnValue := $tmp; ()"""
        merger.append((true, true))

        List(met)
      }
      case Wait(cde) => {
        val met: OpenCode[Unit] = code"$timer := ($timer!) + $cde; ()"
        merger.append((true, false))

        List(met)
      }
      case send: Send[c] => {
        import send.R
        //code"${msg.mtd.sym}"
        val methodId = Const(methodIdMapping.map(_.swap).get(send.msg.mtd.sym).get)

        val f1: OpenCode[Unit] =
          code"""
                    val sender = ${send.actorFrom};
                    val receiver = ${send.actorRef};
                    val argss:List[List[Any]] = ${send.msg.argss};
                    val requestMessage = RequestMessageInter[Any](sender.id, receiver.id, ${methodId}, argss);
                    sender.sendMessage(requestMessage);
                    sender.setMessageResponseHandler(requestMessage.sessionId, (response: _root_.Simulation.Message) => {
                      $responseMessage := response.asInstanceOf[ResponseMessageInter[Any]]
                    })
                    $returnValue := null
                    ()
              """
        val f2: OpenCode[Unit] =
          code"""
                        $timer := ($timer!) + 1;
                        ()
                      """
        val f3: OpenCode[Unit] = code"""if(($responseMessage!) == null) {$pos := (($pos!) - 2);}; ()"""

        val f4: OpenCode[Unit] =
          code"""
                       $returnValue := ($responseMessage!).arg;
                       $responseMessage := null;
                       ()"""


        if (send.msg.mtd.blocking) {
          merger.append((true, true))
          merger.append((true, false))
          merger.append((true, false))
          merger.append((true, true))
          List(f1, f2, f3, f4)
        } else {
          merger.append((true, true))
          List(f1)
        }
      }
      case CallMethodC(methodId, argss) => {
        //What we have to do:
        // 1. Push next position on stack
        // 2. Set Parameters and save current params of method
        // 2. Jump to position where method is located
        val f1: OpenCode[Unit] =
        code"""
          $posSafer := (($pos!) + 1) :: ($posSafer!);
          val tmpArgs = ${argss}.asInstanceOf[List[List[Any]]].flatten
          val currentList = ($methodVariableTableVarStack!).get($methodId).get!
          val newStackVal:List[List[Any]] = (tmpArgs :: currentList)
          $methodVariableTableVarStack!($methodId) := newStackVal;

          val tmpMethodArgs:List[MutVar[Any]] = $methodVariableTableVar!($methodId)!
          val merged = tmpMethodArgs zip tmpArgs
          for(x <- merged) {
            x._1 := x._2
          }
          $pos := (($methodLookupTableVar!($methodId)) - 1);
          ()
          """
        // 3. Method will return to position pushed on stack and contain returnValue
        // 4. Restore save variable from stack
        val f2: OpenCode[Unit] =
        code"""
          val currentList = ($methodVariableTableVarStack!).get($methodId).get.!;
          ($methodVariableTableVarStack!)($methodId) := currentList.tail;

          if (!currentList.tail.isEmpty) {
            val tmpMethodArgs:List[MutVar[Any]] = $methodVariableTableVar!($methodId)!
            val merged = tmpMethodArgs zip currentList.tail.head
            for(x <- merged) {
              x._1 := x._2
            }
          }

          ()
          """
        merger.append((true, false))
        merger.append((true, true))
        List(f1, f2)
      }
      case CallMethod(sym, argss) => {
        val methodId = Const(methodIdMapping.map(_.swap).get(sym).get)
        createCodeLogic(CallMethodC(methodId, argss))
      }
      case fe: Foreach[b, _] => {
        import fe.E
        val iter = Variable[Iterator[b]]
        val iterMut = Variable[MutVar[Iterator[b]]]
        val listValMut = Variable[MutVar[b]]

        variables = VarWrapper(iter, iterMut) :: variables
        variables = VarWrapper(fe.variable, listValMut) :: variables

        //Merger of f2 has to be done before calling createCode of f3!!!
        merger.append((true, true))
        merger.append((false, false))

        val f1 = code"""$iterMut := ${fe.ls}.iterator; ()"""


        val tmp = merger

        // sub-merging here required because of f3.length access to jump to correct position
        merger = ListBuffer()
        val f3_0 = code"""$pos := ((($posSafer!).head) - 1); $posSafer := ($posSafer!).tail; ()"""
        val f3_1 = createCodeLogic(fe.f) ::: List(f3_0)
        merger.append((true, false)) //f3_0
        val f3 = mergeCodes(f3_1, merger.toList)

        merger = tmp
        mergeMerger(f3)

        val f2 = code"""if($iter.hasNext) {$posSafer := ($pos!) :: ($posSafer!); $listValMut := $iter.next;} else {$pos := ($pos!) + ${Const(f3.length)};}"""


        (List(f1, f2) ::: f3).map(x => x.subs(iter).~>(code"($iterMut!)")).map(x => x.subs(fe.variable).~>(code"($listValMut!)"))
      }
      case lb: LetBinding[v, a] => {
        import lb.V

        val met1 = createCodeLogic(lb.value)

        if (lb.bound.isEmpty) {
          val met2 = createCodeLogic(lb.rest)
          met1 ::: met2
        } else {
          var bindingMut = Variable[MutVar[v]]
          var contained = false

          val finding = varSavers.find(_.from == lb.bound.get)
          if (finding.isDefined) {
            bindingMut = finding.get.to.asInstanceOf[Variable[MutVar[v]]]
            contained = true
          } else {
            val tmp = VarWrapper(lb.bound.get, bindingMut)
            variables = tmp :: variables
            varSavers = tmp :: varSavers
          }

          val bindingMutFinal = bindingMut

          val met2 = code"""$bindingMutFinal := (($returnValue!).asInstanceOf[v]); ()"""
          merger.append((true, true))
          val bound = lb.bound.get
          val met3 = createCodeLogic(lb.rest).map(x => x.subs(bound).~>(code"($bindingMutFinal!)"))

          if (!contained) {
            varSavers = varSavers.filter(_.from != lb.bound)
          }

          met1 ::: List(met2) ::: met3
        }
      }
      case If(cond, body) => {
        //Append for met1 before calling met2
        merger.append((true, false))

        val tmp = merger
        merger = ListBuffer()
        val met2_1 = createCodeLogic(body)
        val met2 = mergeCodes(met2_1, merger.toList)
        merger = tmp
        mergeMerger(met2)

        val met1 = code"""if(!$cond) {$pos := ($pos!) + ${Const(met2.length)};}"""

        List(met1) ::: met2
      }
    }
  }
}
