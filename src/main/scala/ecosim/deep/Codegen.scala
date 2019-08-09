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
    val f1 = code"$pos := ($posSafer!).head; $posSafer := ($posSafer!).tail; ()"
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

        val f1: OpenCode[Unit] = code"""$posSafer := ($pos!) :: ($posSafer!); $pos := ($pos!) + 1; ()"""
        val f2: OpenCode[Unit] = code"""$pos := ($posSafer!).head; $posSafer := ($posSafer!).tail; ()"""
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
        val met: OpenCode[Unit] = code"""$pos := ($pos!) + 1; $returnValue := $tmp(); ()"""

        code"""List(() => $met)"""
      }
      case Wait(cde) => {
        val met: OpenCode[Unit] = code"$pos := ($pos!) + 1; $timer := ($timer!) + $cde; ()"

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
                   $pos := ($pos!) + 1;
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
                        $pos := ($pos!) + 1;
                        $timer := ($timer!) + 1;
                        ()
                      """
        val f3: OpenCode[Unit] = code"""$pos := ($pos!) + 1; if(($responseMessage!) == null) {$pos := ($pos!) - 2;}; ()"""

        val f4: OpenCode[Unit] =code"""
                       $pos := ($pos!) + 1;
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
        val f1: OpenCode[Unit] = code""" $pos := ($pos!) + 1; $posSafer := ($pos!) :: ($posSafer!); $methodVariableTableVar!($methodId) :=  $arg; $pos := $methodLookupTableVar!($methodId); ()"""
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
        val f1: OpenCode[Unit] = code""" $pos := ($pos!) + 1; $posSafer := ($pos!) :: ($posSafer!); $methodVariableTableVar!($methodId) :=  $arg; $pos := $methodLookupTableVar!($methodId); ()"""
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

        val f1 = code"""$pos := ($pos!) + 1; $iterMut := ${fe.ls}.iterator; ()"""
        val reset = code"""$pos := ($posSafer!).head; $posSafer := ($posSafer!).tail; ()"""
        val f3 = code"""${createCode(fe.f(listVal))} ::: List[() => Unit](() => $reset)"""
        val f2 = code"""if($iter.hasNext) {$posSafer := ($pos!) :: ($posSafer!); $listValMut := $iter.next; $pos := ($pos!) + 1;} else {$pos := ($pos!) + 1 + $f3.length;}"""

        val finalCode = code"""List(() => $f1, () => $f2) ::: $f3"""
        val mut1 = finalCode.subs(iter)~>(code"($iterMut!).asInstanceOf[Iterator[Any]]")
        val mut2 = mut1.subs(listVal)~>(code"($listValMut!).asInstanceOf[b]")
        mut2
      }
      case lb: LetBinding[v, a] => {
        import lb.V

        //TODO: solve rebindings to same var as not defining new var, if in same method
        val bindingMut = Variable[MutVar[Any]]
        variables = VarWrapper(lb.bound.asInstanceOf[Variable[Any]], bindingMut, code"null") :: variables

        val met: OpenCode[Unit] = code"""$bindingMut := ${lb.value};  $pos := ($pos!) + 1; ()"""

        val met2 = createCode(lb.body).subs(lb.bound).~>(code"($bindingMut!).asInstanceOf[v]")
        code"""List(() => $met) ::: $met2"""
      }
      case lb: LetBinding2[v, a] => {
        import lb.V

        //TODO: solve rebindings to same var as not defining new var, if in same method
        val bindingMut2 = Variable[MutVar[Any]]
        variables = VarWrapper(lb.bound.asInstanceOf[Variable[Any]], bindingMut2, code"null") :: variables

        val met1 = createCode(lb.value)
        val met2 = code"""$bindingMut2 := ($returnValue!);  $pos := ($pos!) + 1; ()"""
        val met3 = createCode(lb.body).subs(lb.bound).~>(code"($bindingMut2!).asInstanceOf[v]")

        code"""$met1 ::: List(() => $met2) ::: $met3"""
      }
    }
  }

}
