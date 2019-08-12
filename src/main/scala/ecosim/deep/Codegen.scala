package ecosim.deep

import IR.Predef._
import squid.lib.MutVar
import _root_.Simulation.{RequestMessageInter, ResponseMessageInter}
import code.__wait

class Codegen[X <: ecosim.runtime.Actor](methodIdMapping: Map[Int, IR.MtdSymbol], actorType: ActorType[X])(implicit val X: CodeType[X]) {

  val timer = Variable[MutVar[Int]]
  val pos = Variable[MutVar[Int]]

  val posSafer = Variable[MutVar[List[Int]]]
  val returnValue = Variable[MutVar[Any]]

  val responseMessage = Variable[MutVar[ResponseMessageInter[Any]]]

  case class VarWrapper(val from: Variable[Any], val to:Variable[MutVar[Any]], val init: OpenCode[Any])
  var variables:List[VarWrapper] = List()

  def generateMutVarInit[R: CodeType](variables:List[VarWrapper], after: OpenCode[R]): OpenCode[R] = variables match {
    case Nil => code"$after"
    case (x::xs) => code"val ${x.to} = MutVar(${x.init}); ${generateMutVarInit(xs, after)}"
  }

  def generateMethodTable(data: List[(OpenCode[List[() => Unit]], Variable[_], Int)], pos: OpenCode[Int]): OpenCode[Map[Int, Int]] = data match{
    case (x::xs) => code"""val l = ${x._1}.length; $${generateMethodTable(xs, code"$$pos + l")} + (${Const(x._3)} -> $pos)"""
    case Nil => code"Map[Int, Int]()"
  }

  def generateVariableTable(data: List[(OpenCode[List[() => Unit]], Variable[MutVar[Any]], Int)]): OpenCode[Map[Int, MutVar[Any]]] = data match{
    case (x::xs) => code"""$${generateVariableTable(xs)} + (${Const(x._3)} -> ${x._2})"""
    case Nil => code"Map[Int, MutVar[Any]]()"
  }

  private def codeGenMethod[A](method: Method[A,_]): (OpenCode[List[() => Unit]], Variable[MutVar[Any]], Int) = {
    import method.A
    val methodArgs = Variable[A];
    val methodArgsMut = Variable[MutVar[Any]];
    val methodAlgo = method.body(methodArgs)
    val f1 = code"$pos := ((($posSafer!).head) - 1); $posSafer := ($posSafer!).tail; ()"
    variables = VarWrapper(methodArgs.asInstanceOf[Variable[Any]], methodArgsMut, code"null") :: variables
    (code"""${this.createCode(methodAlgo).subs(methodArgs).~>(code"($methodArgsMut!).asInstanceOf[A]")} ::: List[() => Unit](() => $f1)""", methodArgsMut, methodIdMapping.map(_.swap).get(method.sym).get)
  }

  var methodLookupTableVar = Variable[MutVar[Map[Int, Int]]]
  var methodLookupTable: OpenCode[Map[Int,Int]] = code"Map[Int, Int]()"
  var methodVariableTable: OpenCode[Map[Int,MutVar[Any]]] = code"Map[Int, MutVar[Any]]()"
  var methodVariableTableVar =  Variable[MutVar[Map[Int, MutVar[Any]]]]


  def compile(selfRef: X): (Int, Int, Int) => (Int, Int) = {

    val methodCodes = this.actorType.methods.map(mtd => codeGenMethod(mtd))
    val main = this.createCode(this.actorType.main)
    val commands = methodCodes.foldLeft(main)((x,y) => code"$x ::: ${y._1}")

    methodLookupTable = generateMethodTable(methodCodes, code"$main.length")

    methodVariableTable = generateVariableTable(methodCodes)

    val finalCode = this.createExec(this.actorType.self, commands)

    val f = finalCode.compile
    f(selfRef)
  }


  var varSavers = Map[Variable[_], Variable[MutVar[Any]]]()




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
//        val y = createCode(Block(bdy.tail: _*))
        val e = code"""List[() => Unit](() => $f2)"""

        val result = code"$a ::: $x ::: $e"
        result
      }
//      case Block(bdy@_*) => {
//        if (bdy.isEmpty) {
//          code"Nil"
//        } else {
//          val listVar = Variable[MutVar[List[Any]]]
//
//          val x = createCode(bdy.head)
//          val y = createCode(Block(bdy.tail: _*))
//
//          code"$x ::: $y"
//        }
//      }
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
        val iter = Variable[Iterator[Any]]
        val iterMut = Variable[MutVar[Any]]
        val listVal = Variable[b]
        val listValMut = Variable[MutVar[Any]]

        variables = VarWrapper(iter.asInstanceOf[Variable[Any]], iterMut, code"null") :: variables
        variables = VarWrapper(listVal.asInstanceOf[Variable[Any]], listValMut, code"null") :: variables

        val f1 = code"""$iterMut := ${fe.ls}.iterator; ()"""
        val reset = code"""$pos := ((($posSafer!).head) - 1); $posSafer := ($posSafer!).tail; ()"""
        val f3 = code"""${createCode(fe.f(listVal))} ::: List[() => Unit](() => $reset)"""
        val f2 = code"""if($iter.hasNext) {$posSafer := ($pos!) :: ($posSafer!); $listValMut := $iter.next;} else {$pos := ($pos!) + $f3.length;}"""

        val finalCode = code"""List(() => $f1, () => $f2) ::: $f3"""
        val mut1 = finalCode.subs(iter)~>(code"($iterMut!).asInstanceOf[Iterator[Any]]")
        val mut2 = mut1.subs(listVal)~>(code"($listValMut!).asInstanceOf[b]")
        mut2
      }
//      case lb: LetBinding[v, a] => {
//        import lb.V
//
//        var bindingMut = Variable[MutVar[Any]]
//
//        var contained = false
//        if (varSavers.contains(lb.bound)) {
//          bindingMut = varSavers(lb.bound)
//          contained = true
//        } else {
//          variables = VarWrapper(lb.bound.asInstanceOf[Variable[Any]], bindingMut, code"null") :: variables
//          varSavers = varSavers + (lb.bound -> bindingMut)
//        }
//
//        val bindingMutFinal = bindingMut
//
//        val met: OpenCode[Unit] = code"""$bindingMutFinal := ${lb.value}; ()"""
//        val met2 = createCode(lb.body).subs(lb.bound).~>(code"($bindingMutFinal!).asInstanceOf[v]")
//
//        if (!contained) {
//          varSavers = varSavers.filter(_._1 != lb.bound)
//        }
//
//
//        code"""List(() => $met) ::: $met2"""
//      }
      case lb: LetBinding[v, a] => {
        import lb.V

        var bindingMut2 = Variable[MutVar[Any]]
        var contained = false

        if (varSavers.contains(lb.bound)) {
          bindingMut2 = varSavers(lb.bound)
          contained = true
        } else {
          variables = VarWrapper(lb.bound.asInstanceOf[Variable[Any]], bindingMut2, code"null") :: variables
          varSavers = varSavers + (lb.bound -> bindingMut2)
        }

        val bindingMutFinal2 = bindingMut2

        val met1 = createCode(lb.value)
        val met2 = code"""$bindingMutFinal2 := ($returnValue!); ()"""
        val met3 = createCode(lb.body).subs(lb.bound).~>(code"($bindingMutFinal2!).asInstanceOf[v]")

        code"""$met1 ::: List(() => $met2) ::: $met3"""
      }
    }
  }

}
