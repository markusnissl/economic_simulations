package ecosim.runtime

import Simulation.{RequestMessage, Message, SimO}
import code.{Instruction, __do}

class Actor extends SimO {
  override def mycopy(): SimO = {
    new Actor()
  }
  private[ecosim] var owner: Simulation = _

  override def algo: Instruction = __do{}

  setMessageHandler("RequestMessage", (message: Message) => {
    val mCast:RequestMessage = message.asInstanceOf[RequestMessage]
    val result = mCast.call_f(this)
    mCast.reply(this, result)
  })

}
class Simulation {
  
}
