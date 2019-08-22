package ecosim.deep.algo

import ecosim.deep.IR
import ecosim.deep.IR.Predef._
import simulation.core.{RequestMessage, ResponseMessage}
import simulation.core.Actor

case class Send[R](actorFrom: OpenCode[Actor],
                   actorRef: OpenCode[Actor],
                   methodId: Int,
                   argss: List[List[OpenCode[_]]],
                   blocking: Boolean)(implicit val R: CodeType[R]) extends Algo[R] {

  override def codegen: List[IR.Predef.OpenCode[Unit]] = {
    val methodIdC = Const(methodId)

    val initCodeO: OpenCode[List[List[Any]]] = code"Nil"
    //Convert args, so that the can be used inside of simulation.generated codes
    val convertedArgs: OpenCode[List[List[Any]]] = argss.foldRight(initCodeO)((x, y) => {
      val initCode: OpenCode[List[Any]] = code"Nil"
      val z: OpenCode[List[Any]] = x.foldRight(initCode)((a, b) => code"$a::$b")
      code"$z::$y"
    })

    val f1: OpenCode[Unit] =
      code"""
                    val sender = ${actorFrom};
                    val receiver = ${actorRef};
                    val requestMessage = RequestMessage(sender.id, receiver.id, ${methodIdC}, $convertedArgs);
                    sender.sendMessage(requestMessage);
                    sender.setMessageResponseHandler(requestMessage.sessionId, (response: simulation.core.Message) => {
                      ${AlgoInfo.responseMessage} := response.asInstanceOf[ResponseMessage]
                    })
                    ${AlgoInfo.returnValue} := null
                    ()
              """
    val f2: OpenCode[Unit] =
      code"""
                        ${AlgoInfo.timeVar} := (${AlgoInfo.timeVar}!) + 1;
                        ()
                      """
    val f3: OpenCode[Unit] = code"""if((${AlgoInfo.responseMessage}!) == null) {${AlgoInfo.jump(-2)}}; ()"""

    val f4: OpenCode[Unit] =
      code"""
                       ${AlgoInfo.returnValue} := (${AlgoInfo.responseMessage}!).arg;
                       ${AlgoInfo.responseMessage} := null;
                       ()"""


    if (blocking) {
      AlgoInfo.merger.append((true, true))
      AlgoInfo.merger.append((false, false))
      AlgoInfo.merger.append((true, false))
      AlgoInfo.merger.append((true, true))
      List(f1, f2, f3, f4)
    } else {
      AlgoInfo.merger.append((true, true))
      List(f1)
    }
  }
}
