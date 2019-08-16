package ecosim.deep

import IR.Predef._
import squid.lib.MutVar
import simulation.{RequestMessageInter, ResponseMessageInter}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class Codegen[X <: ecosim.runtime.Actor](methodIdMapping: Map[IR.MtdSymbol, Int], actorType: ActorType[X])(implicit val X: CodeType[X]) {

  case class VarWrapper[C](val from: Variable[C], val to: Variable[MutVar[C]])(implicit val A: CodeType[C])
  case class VarValue[C](val variable: Variable[C], val init: OpenCode[C])(implicit val A: CodeType[C])


  val timer = Variable[MutVar[Int]]
  val pos = Variable[MutVar[Int]]

  val posSafer = Variable[MutVar[List[Int]]]
  val returnValue = Variable[MutVar[Any]]

  val responseMessage = Variable[MutVar[ResponseMessageInter[Any]]]

  val methodLookupTable: collection.mutable.Map[Int, Int] = collection.mutable.Map[Int, Int]()

  var methodVariableTable: collection.mutable.Map[Int, ArrayBuffer[Variable[MutVar[Any]]]] = collection.mutable.Map[Int, ArrayBuffer[Variable[MutVar[Any]]]]()

  var methodVariableTableStack: collection.mutable.Map[Int, ArrayBuffer[Variable[ListBuffer[Any]]]] = collection.mutable.Map[Int, ArrayBuffer[Variable[ListBuffer[Any]]]]()

  var variables: List[VarWrapper[_]] = List()
  var variables2: List[VarValue[_]] = List()

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

  def initVar2[A, R: CodeType](variable: VarValue[A], rest: OpenCode[R]): OpenCode[R] = {
    import variable.A
    code"val ${variable.variable} = ${variable.init}; $rest"
  }

  def generateVarInit[R: CodeType](variables: List[VarValue[_]], after: OpenCode[R]): OpenCode[R] = variables match {
    case Nil => code"$after"
    case (x :: xs) => {
     initVar2(x, generateVarInit(xs, after))
    }
  }

  def generateMethodTable(methodData: List[(Int, Int)], currentPos: Int): Unit = methodData match {
    case (x :: xs) => {
      methodLookupTable(x._1) = currentPos
      generateMethodTable(xs, currentPos + x._2)
    }
    case Nil => ()
  }

  def generateVariableTable(data: List[(List[Variable[MutVar[Any]]], Int)]): Unit = data match {
    case (x :: xs) => {
      methodVariableTable(x._2) = ArrayBuffer(x._1:_*)
      generateVariableTable(xs)
    }
    case Nil => ()
  }

  def generateVariableTableStack(data: List[(Int, Int)]): Unit = data match {
    case (x :: xs) => {
      val a = ArrayBuffer[Variable[ListBuffer[Any]]]()
      for (i <- 0 until x._2) {
        val x = Variable[ListBuffer[Any]]
        a.append(x)
        variables2 = VarValue(x, code"ListBuffer[Any]()") :: variables2
      }
      methodVariableTableStack(x._1) = a
      generateVariableTableStack(xs)
    }
    case Nil => ()
  }

  //This is not supported :(
  //var testMethMap:ClosedCode[Map[Int, Variable[MutVar[_]]]] = code"Map[Int,Variable[MutVar[_]]]()"

  private def codeGenMethod(method: LiftedMethod[_]): (OpenCode[List[() => Unit]], List[Variable[MutVar[Any]]], Int, Int) = {

    //FIXME: Cannot create a map in squid with _ param, therefore have to use any
    // Cannot define map outside, since methods may have recursive dependency, so lookup has to be made inside squid

    var (code, codeLength) = this.createCode(method.body, true)

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
    (code, varList.toList, methodIdMapping(method.sym), codeLength)
  }


  def compile: (X) => (Int, Int, Int) => (Int, Int) = {

    val methodCodes = this.actorType.methods.map(mtd => codeGenMethod(mtd))
    val (main, mainLength) = this.createCode(this.actorType.main)

    val commands = methodCodes.foldLeft(main)((x, y) => code"$x ::: ${y._1}")

    generateMethodTable(methodCodes.map(x => (x._3, x._4)), mainLength)
    generateVariableTable(methodCodes.map(x=> (x._2, x._3)))
    generateVariableTableStack(methodCodes.map(x => (x._3, x._2.length)))

    val commandsRewrite = commands.rewrite({
      case code"getMethodPosition(${Const(a)})" => Const(methodLookupTable(a))
      case code"setMethodParam(${Const(a)}, ${Const(b)}, $c)" => {
        val variable:Variable[MutVar[Any]] = methodVariableTable(a)(b)
        code"$variable := $c"
      }
      case code"saveMethodParam(${Const(a)}, ${Const(b)}, $c)" => {
        val stack:ArrayBuffer[Variable[ListBuffer[Any]]] = methodVariableTableStack(a)
        val varstack:Variable[ListBuffer[Any]] = stack(b)
        code"$varstack.prepend($c);"
      }
      case code"restoreMethodParams(${Const(a)})" => {
        val stack:ArrayBuffer[Variable[ListBuffer[Any]]] = methodVariableTableStack(a)
        val initCode:OpenCode[Unit] = code"()"
        stack.zipWithIndex.foldRight(initCode)((c,b) => {
          val variable:Variable[MutVar[Any]] = methodVariableTable(a)(c._2)
          val ab = c._1
          code"$ab.remove(0); if(!$ab.isEmpty) {$variable := $ab(0)}; $b; ()"
        })
      }
    })

    val finalCode = this.createExec(this.actorType.self, commandsRewrite)

    println(finalCode)
    println(IR.showScala(finalCode.rep))
    val f = finalCode.compile
    f
  }


  def createExec(selfVar: Variable[X], commands: OpenCode[List[() => Unit]]): ClosedCode[(X) => (Int, Int, Int) => (Int, Int)] = {
    generateMutVarInit(variables,generateVarInit(variables2,
      code"""(self:X) => {
         val $returnValue = MutVar(null)
         val $posSafer = MutVar(List())
         val $selfVar = self
         val $responseMessage = MutVar(null)
         val $timer = MutVar(0)
         val $pos = MutVar(0)

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
      """)).unsafe_asClosedCode
  }

  def createCode(algo: Algo[_], isMethod: Boolean = false): (OpenCode[List[() => Unit]], Int) = {
    merger.clear()
    var commands: List[OpenCode[Unit]] = createCodeLogic(algo)
    if (isMethod) {
      val f1 = code"$pos := ((($posSafer!).head) - 1); $posSafer := ($posSafer!).tail; ()"
      commands = commands ::: List(f1)
      merger.append((true, false))
    }

    commands = mergeCodes(commands, merger.toList)

    val start: OpenCode[List[() => Unit]] = code"List[() => Unit]()"
    val code = commands.map(x => code"List(() => ${x})").foldLeft(start)((x, y) => code"$x ::: $y")
    (code, commands.length)
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
        val methodId = Const(methodIdMapping(send.msg.mtd.sym))

        val initCodeO:OpenCode[List[List[Any]]]=code"Nil"
        //Convert args, so that the can be used inside of generated codes
        val convertedArgs:OpenCode[List[List[Any]]] = send.msg.argss.foldRight(initCodeO)((x,y) => {
          val initCode:OpenCode[List[Any]]=code"Nil"
          val z:OpenCode[List[Any]] = x.foldRight(initCode)((a,b) => code"$a::$b")
          code"$z::$y"
        })

        val f1: OpenCode[Unit] =
          code"""
                    val sender = ${send.actorFrom};
                    val receiver = ${send.actorRef};
                    val requestMessage = RequestMessageInter(sender.id, receiver.id, ${methodId}, $convertedArgs);
                    sender.sendMessage(requestMessage);
                    sender.setMessageResponseHandler(requestMessage.sessionId, (response: simulation.Message) => {
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
      case CallMethod(methodId, argss) => {
        //What we have to do:
        // 1. Push next position on stack
        // 2. Set Parameters and save current params of method

        // 2. Jump to position where method is located

        val initParam:OpenCode[Any] = code"()"

        val flattendArgs = argss.flatten
        val setMethodParamsList = flattendArgs.zipWithIndex.foldRight(initParam)((a,b) => code"setMethodParam(${Const(methodId)}, ${Const(a._2)}, ${a._1}); $b")
        val saveMethodParamsList = flattendArgs.zipWithIndex.foldRight(initParam)((a,b) => code"saveMethodParam(${Const(methodId)}, ${Const(a._2)}, ${a._1}); $b")
        val restoreMethodParam = code"restoreMethodParams(${Const(methodId)})"


        val f1: OpenCode[Unit] =
          code"""$posSafer := (($pos!) + 1) :: ($posSafer!);
                 $saveMethodParamsList;
                 $setMethodParamsList;

                 $pos := (getMethodPosition(${Const(methodId)}) - 1);
                 ()
            """
        // 3. Method will return to position pushed on stack and contain returnValue
        // 4. Restore save variable from stack
        val f2: OpenCode[Unit] =
        code"""
               $restoreMethodParam
          ()
          """
        merger.append((true, false))
        merger.append((true, true))
        List(f1, f2)
      }
      case CallMethodDebug(sym, argss) => {
        createCodeLogic(CallMethod(methodIdMapping(sym), argss))
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
      case IfElse(cond, ifBody, elseBody) => {
        //Append for met1 before calling met2
        merger.append((true, false))

        var tmp = merger
        merger = ListBuffer()
        val met2_1 = createCodeLogic(ifBody)
        val met2 = mergeCodes(met2_1, merger.toList)
        merger = tmp
        mergeMerger(met2)

        //Append for metInner before calling met3
        merger.append((true, false))

        tmp = merger
        merger = ListBuffer()
        val met3_1 = createCodeLogic(elseBody)
        val met3 = mergeCodes(met3_1, merger.toList)
        merger = tmp
        mergeMerger(met3)

        val met1 = code"""if(!$cond) {$pos := ($pos!) + ${Const(met2.length + 1)};}"""
        val metInner = code"""$pos := ($pos!) + ${Const(met3.length)}; ()"""

        List(met1) ::: met2 ::: List(metInner) ::: met3
      }
      case NoOp() => List()
    }
  }
}
