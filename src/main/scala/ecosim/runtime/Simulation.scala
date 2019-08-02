package ecosim.runtime


import Simulation.{Message, RequestMessageInter, SimO}
import code.{Instruction, __do}

class Actor extends SimO {
  override def mycopy(): SimO = {
    new Actor()
  }
  private[ecosim] var owner: Simulation = _

  override def algo: Instruction = __do{}

  setMessageHandler("RequestMessageInter", (message: Message) => {
    //Ignore warning, handled by fragment in logic
    //TODO: remove code from here
    val mCast = message.asInstanceOf[RequestMessageInter[Any,Any]]
    mCast.reply(this, null)
  })

  setMessageHandler("ResponseMessageInter", (message: Message) => {
    //Ignore warning, handled by fragment in logic
  })

}
class Simulation {
  
}
