package ecosim.runtime


import Simulation.{Message, RequestMessageInter, SimO}
import code.{Instruction, __do}

class Actor extends SimO {
  override def mycopy(): SimO = {
    new Actor()
  }
  private[ecosim] var owner: Simulation = _

  override def algo: Instruction = __do{}

  var returnValue = false
  setMessageHandler("RequestMessageInter", (message: Message) => {
    //Ignore warning, handled by fragment in logic
    //TODO: remove code from here
    val mCast = message.asInstanceOf[RequestMessageInter[Any,Any]]
    returnValue = !returnValue
    println("returning", returnValue)
    mCast.reply(this, returnValue)
  })

  setMessageHandler("ResponseMessageInter", (message: Message) => {
    //Ignore warning, handled by fragment in logic
  })

}
class Simulation {
  
}
