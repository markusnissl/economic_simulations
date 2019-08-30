package ecosim.deep.algo

import ecosim.deep.IR
import ecosim.deep.IR.Predef._
import ecosim.deep.member.{Actor, RequestMessage, ResponseMessage}

case class Send[R](actorFrom: OpenCode[Actor],
                   actorRef: OpenCode[Actor],
                   methodId: Int,
                   argss: List[List[OpenCode[_]]],
                   blocking: Boolean)(implicit val R: CodeType[R]) extends Algo[R] {

  override def codegen: Unit = {
    val methodIdC = Const(methodId)

    // Convert arguments to opencode, so hat the can be used as argument inside of OpenCode
    val initCodeO: OpenCode[List[List[Any]]] = code"Nil"
    val convertedArgs: OpenCode[List[List[Any]]] = argss.foldRight(initCodeO)((x, y) => {
      val initCode: OpenCode[List[Any]] = code"Nil"
      val z: OpenCode[List[Any]] = x.foldRight(initCode)((a, b) => code"$a::$b")
      code"$z::$y"
    })


    if (blocking) {
      // 1. Send message and register response handler, which stores the response in a variable
      // 2. Increase timer, so that you wait for some time
      // 3. If message has not been received, jump to timer and wait again for the next step
      // 4. Asap message is received, set return value and reset responseMessage to null, so that it can be used the next time again
      val f1: OpenCode[Unit] =
      code"""
                    val sender = ${actorFrom};
                    val receiver = ${actorRef};
                    val requestMessage = RequestMessage(sender.id, receiver.id, ${methodIdC}, $convertedArgs);
                    sender.sendMessage(requestMessage);
                    sender.setMessageResponseHandler(requestMessage.sessionId, (response: ecosim.deep.member.Message) => {
                      ${AlgoInfo.responseMessage} := response.asInstanceOf[ResponseMessage]
                    })
                    ${AlgoInfo.returnValue} := null
                    ()
              """
      val f2: OpenCode[Unit] = code"""()"""
      val f3: OpenCode[Unit] = code"""()"""
      val f4: OpenCode[Unit] =
        code"""
                       ${AlgoInfo.returnValue} := (${AlgoInfo.responseMessage}!).arg;
                       ${AlgoInfo.responseMessage} := null;
                       ()"""

      AlgoInfo.stateGraph.append(AlgoInfo.EdgeInfo("Send b f1",AlgoInfo.CodeNodePos(AlgoInfo.posCounter), AlgoInfo.CodeNodePos(AlgoInfo.posCounter+1), f1))
      AlgoInfo.nextPos
      AlgoInfo.stateGraph.append(AlgoInfo.EdgeInfo("Send b f2",AlgoInfo.CodeNodePos(AlgoInfo.posCounter), AlgoInfo.CodeNodePos(AlgoInfo.posCounter+1), f2, waitEdge = true))
      AlgoInfo.nextPos
      AlgoInfo.stateGraph.append(AlgoInfo.EdgeInfo("Send b f3 result",AlgoInfo.CodeNodePos(AlgoInfo.posCounter), AlgoInfo.CodeNodePos(AlgoInfo.posCounter+1), f3, cond=code"(${AlgoInfo.responseMessage}!) != null"))
      AlgoInfo.stateGraph.append(AlgoInfo.EdgeInfo("Send b f3 no result",AlgoInfo.CodeNodePos(AlgoInfo.posCounter), AlgoInfo.CodeNodePos(AlgoInfo.posCounter-1), f3, cond=code"(${AlgoInfo.responseMessage}!) == null"))
      AlgoInfo.nextPos
      AlgoInfo.stateGraph.append(AlgoInfo.EdgeInfo("Send b f4",AlgoInfo.CodeNodePos(AlgoInfo.posCounter), AlgoInfo.CodeNodePos(AlgoInfo.posCounter+1), f4))
      AlgoInfo.nextPos
    } else {
      val f1: OpenCode[Unit] =
        code"""
                    val sender = ${actorFrom};
                    val receiver = ${actorRef};
                    val requestMessage = RequestMessage(sender.id, receiver.id, ${methodIdC}, $convertedArgs);
                    sender.sendMessage(requestMessage);
                    ${AlgoInfo.returnValue} := null
                    ()
              """
      AlgoInfo.stateGraph.append(AlgoInfo.EdgeInfo("Send nb f1",AlgoInfo.CodeNodePos(AlgoInfo.posCounter), AlgoInfo.CodeNodePos(AlgoInfo.posCounter+1), f1))
      AlgoInfo.nextPos
    }
  }
}
