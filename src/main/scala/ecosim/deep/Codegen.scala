package ecosim.deep

import IR.Predef._
import squid.lib.MutVar
import _root_.Simulation.{RequestMessageInter, ResponseMessageInter}
import code.__wait

class Codegen[X <: ecosim.runtime.Actor](methodIdMapping: Map[Int, IR.MtdSymbol], actorType: ActorType[X])(implicit val X: CodeType[X]) {

  case class VarWrapper[C](val from: Variable[C], val to:Variable[MutVar[C]])(implicit val A: CodeType[C])


  val timer = Variable[MutVar[Int]]
  val pos = Variable[MutVar[Int]]

  val posSafer = Variable[MutVar[List[Int]]]
  val returnValue = Variable[MutVar[Any]]

  val responseMessage = Variable[MutVar[ResponseMessageInter[Any]]]

  var methodLookupTableVar = Variable[MutVar[Map[Int, Int]]]
  var methodLookupTable: OpenCode[Map[Int,Int]] = code"Map[Int, Int]()"
  var methodVariableTable: OpenCode[Map[Int,MutVar[Any]]] = code"Map[Int, MutVar[Any]]()"
  var methodVariableTableVar = Variable[MutVar[Map[Int, MutVar[Any]]]]

  var variables:List[VarWrapper[_]] = List()

  var varSavers = List[VarWrapper[_]]()


  def initVar[A, R:CodeType](variable: VarWrapper[A], rest:OpenCode[R]): OpenCode[R] = {
    import variable.A
    code"val ${variable.to} = ((MutVar[Any](null)).asInstanceOf[MutVar[A]]); $rest"
  }
  def generateMutVarInit[R: CodeType](variables:List[VarWrapper[_]], after: OpenCode[R]): OpenCode[R] = variables match {
    case Nil => code"$after"
    case (x::xs) => {
      initVar(x, generateMutVarInit(xs, after))
    }
  }

  def generateMethodTable(data: List[(OpenCode[List[() => Unit]], Variable[_], Int)], pos: OpenCode[Int]): OpenCode[Map[Int, Int]] = data match{
    case (x::xs) => code"""val l = ${x._1}.length; $${generateMethodTable(xs, code"$$pos + l")} + (${Const(x._3)} -> $pos)"""
    case Nil => code"Map[Int, Int]()"
  }

  def getMap[A:CodeType](variable: Variable[MutVar[A]], pos: Int): OpenCode[Map[Int, MutVar[A]]] = {
    code"Map(${Const(pos)} -> ${variable})"
  }

  def generateVariableTable(data: List[(OpenCode[List[() => Unit]], Variable[MutVar[Any]], Int)]): OpenCode[Map[Int, MutVar[Any]]] = data match{
    case (x::xs) => code"""(${getMap(x._2, x._3)}) ++ $${generateVariableTable(xs)}"""
    case Nil => code"Map[Int, MutVar[Any]]()"
  }

  //This is not supported :(
  //var testMethMap:ClosedCode[Map[Int, Variable[MutVar[_]]]] = code"Map[Int,Variable[MutVar[_]]]()"

  private def codeGenMethod[A](method: Method[A,_]): (OpenCode[List[() => Unit]], Variable[MutVar[Any]], Int) = {
    import method.A

    val methodArgs = Variable[A];
    //FIXME: Cannot create a map in squid with _ param, therefore have to use any
    // Cannot define map outside, since methods may have recursive dependency, so lookup has to be made inside squid

    val methodArgsMut = Variable[MutVar[Any]];
//    val methodArgsMut = Variable[MutVar[A]];
    val methodAlgo = method.body(methodArgs)
    val f1 = code"$pos := ((($posSafer!).head) - 1); $posSafer := ($posSafer!).tail; ()"
    //variables = VarWrapper(methodArgs, methodArgsMut) :: variables
    variables = VarWrapper(methodArgs.asInstanceOf[Variable[Any]], methodArgsMut) :: variables
    (code"""${this.createCode(methodAlgo).subs(methodArgs).~>(code"($methodArgsMut!).asInstanceOf[A]")} ::: List[() => Unit](() => $f1)""", methodArgsMut, methodIdMapping.map(_.swap).get(method.sym).get)
  }

  def compile(selfRef: X): (Int, Int, Int) => (Int, Int) = {

    val methodCodes = this.actorType.methods.map(mtd => codeGenMethod(mtd))
    val main = this.createCode(this.actorType.main)
    val commands = methodCodes.foldLeft(main)((x,y) => code"$x ::: ${y._1}")

    methodLookupTable = generateMethodTable(methodCodes, code"$main.length")
    methodVariableTable = generateVariableTable(methodCodes.map(x => (x._1, x._2.asInstanceOf[Variable[MutVar[Any]]], x._3)))

    val finalCode = this.createExec(this.actorType.self, commands)

    val f = finalCode.compile
    f(selfRef)
  }






  def createExec(selfVar: Variable[X], commands:OpenCode[List[() => Unit]]): ClosedCode[(X) => (Int, Int, Int) => (Int, Int)] = {
    generateMutVarInit(variables, code"""(self:X) => {
         val $returnValue = MutVar(null)
         val $posSafer = MutVar(List())
         val $selfVar = self
         val $responseMessage = MutVar(null)
         val $timer = MutVar(0)
         val $pos = MutVar(0)
         val $methodLookupTableVar = MutVar(Map())
         val $methodVariableTableVar = MutVar(Map())

         $methodLookupTableVar := $methodLookupTable
         $methodVariableTableVar := $methodVariableTable

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


  def createCode(algo: Algo[_]): OpenCode[List[() => Unit]] = {
    algo match {
      case Forever(bdy@_*) => {
        val listVar = Variable[MutVar[List[Any]]]

        val f1: OpenCode[Unit] = code"""$posSafer := ($pos!) :: ($posSafer!); ()"""
        val f2: OpenCode[Unit] = code"""$pos := ((($posSafer!).head) - 1); $posSafer := ($posSafer!).tail; ()"""
        val a = code"""List[() => Unit](() => $f1)"""
        val x = createCode(bdy.head)
        val y = createCode(Block(bdy.tail: _*))
        val e = code"""List[() => Unit](() => $f2)"""

        val result = code"$a ::: $x ::: $y ::: $e"
        result
      }
      case Block(bdy@_*) => {
        if (bdy.isEmpty) {
          code"Nil"
        } else {
          val listVar = Variable[MutVar[List[Any]]]

          val x = createCode(bdy.head)
          val y = createCode(Block(bdy.tail: _*))

          code"$x ::: $y"
        }
      }
      case sc: ScalaCode[a] => {
        import sc.tpe

        val tmp = code"() => ${sc.cde}"
        val met: OpenCode[Unit] = code"""$returnValue := $tmp(); ()"""

        code"""List(() => $met)"""
      }
      case Wait(cde) => {
        val met: OpenCode[Unit] = code"$timer := ($timer!) + $cde; ()"

        code"""List(() => $met)"""
      }
      case send: Send[b, c] => {
        import send.R
        //code"${msg.mtd.sym}"
        val methodId = Const(methodIdMapping.map(_.swap).get(send.msg.mtd.sym).get)

        var blocking = false
        if (send.msg.mtd.isInstanceOf[BlockingMethod[_, _]]) {
          blocking = true
        }

        val f1: OpenCode[Unit] =
          code"""
                    val sender = ${send.actorFrom};
                    val receiver = ${send.actorRef};
                    val arg = ${send.msg.arg};
                    val requestMessage = RequestMessageInter[${send.E},Any](sender.id, receiver.id, ${methodId}, arg);
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

        val f4: OpenCode[Unit] =code"""
                       $returnValue := ($responseMessage!).arg;
                       $responseMessage := null;
                       ()"""


        if (blocking) {
          code"""List(() => $f1, () => $f2, () => $f3, () => $f4)"""
        } else {
          code"""List(() => $f1)"""
        }
      }
      case CallMethodC(methodId, arg) => {
        //What we have to do:
        // 1. Push next position on stack
        // 2. Set Parameters
        // 2. Jump to position where method is located
        val f1: OpenCode[Unit] = code"""$posSafer := (($pos!) + 1) :: ($posSafer!); $methodVariableTableVar!($methodId) :=  $arg; $pos := (($methodLookupTableVar!($methodId)) - 1); ()"""
        // 3. Method will return to position pushed on stack and contain returnValue
        // 4. TODO for later: store and restore variables + dependency analysis to restore only needed ones
        code"""List(() => $f1)"""
      }
      case CallMethod(sym, arg) => {
        //What we have to do:
        // 1. Push next position on stack
        // 2. Set Parameters
        // 3. Jump to position where method is located
        val methodId = Const(methodIdMapping.map(_.swap).get(sym).get)
        val f1: OpenCode[Unit] = code"""$posSafer := (($pos!) + 1) :: ($posSafer!); $methodVariableTableVar!($methodId) :=  $arg; $pos := (($methodLookupTableVar!($methodId)) - 1); ()"""
        // 3. Method will return to position pushed on stack and contain returnValue
        // 4. TODO for later: store and restore variables + dependency analysis to restore only needed ones
        code"""List(() => $f1)"""
      }
      case fe: Foreach[b, _] => {
        import fe.E
        val iter = Variable[Iterator[b]]
        val iterMut = Variable[MutVar[Iterator[b]]]
        val listVal = Variable[b]
        val listValMut = Variable[MutVar[b]]

        variables = VarWrapper(iter, iterMut) :: variables
        variables = VarWrapper(listVal, listValMut) :: variables

        val f1 = code"""$iterMut := ${fe.ls}.iterator; ()"""
        val reset = code"""$pos := ((($posSafer!).head) - 1); $posSafer := ($posSafer!).tail; ()"""
        val f3 = code"""${createCode(fe.f(listVal))} ::: List[() => Unit](() => $reset)"""
        val f2 = code"""if($iter.hasNext) {$posSafer := ($pos!) :: ($posSafer!); $listValMut := $iter.next;} else {$pos := ($pos!) + $f3.length;}"""

        val finalCode = code"""List(() => $f1, () => $f2) ::: $f3"""
        val mut1 = finalCode.subs(iter)~>(code"($iterMut!)")
        val mut2 = mut1.subs(listVal)~>(code"($listValMut!)")
        mut2
      }
      case lb: LetBinding[v, a] => {
        import lb.V

        var bindingMut = Variable[MutVar[v]]

        var contained = false

        val finding = varSavers.find(_.from == lb.bound)
        if (finding.isDefined) {
          bindingMut = finding.get.to.asInstanceOf[Variable[MutVar[v]]]
          contained = true
        } else {
          val tmp = VarWrapper(lb.bound, bindingMut)
          variables = tmp :: variables
          varSavers = tmp :: varSavers
        }

        val bindingMutFinal = bindingMut

        val met: OpenCode[Unit] = code"""$bindingMutFinal := ${lb.value}; ()"""
        val met2 = createCode(lb.body).subs(lb.bound).~>(code"($bindingMutFinal!)")

        if (!contained) {
          varSavers = varSavers.filter(_.from != lb.bound)
        }


        code"""List(() => $met) ::: $met2"""
      }
      case lb: LetBinding2[v, a] => {
        import lb.V

        var bindingMut2 = Variable[MutVar[v]]
        var contained = false

        val finding = varSavers.find(_.from == lb.bound)
        if (finding.isDefined) {
          bindingMut2 = finding.get.to.asInstanceOf[Variable[MutVar[v]]]
          contained = true
        } else {
          val tmp = VarWrapper(lb.bound, bindingMut2)
          variables = tmp :: variables
          varSavers = tmp :: varSavers
        }

        val bindingMutFinal2 = bindingMut2

        val met1 = createCode(lb.value)
        val met2 = code"""$bindingMutFinal2 := (($returnValue!).asInstanceOf[v]); ()"""
        val met3 = createCode(lb.body).subs(lb.bound).~>(code"($bindingMutFinal2!)")

        if (!contained) {
          varSavers = varSavers.filter(_.from != lb.bound)
        }

        code"""$met1 ::: List(() => $met2) ::: $met3"""
      }
    }
  }

}
