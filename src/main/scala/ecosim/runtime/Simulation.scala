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
  })

  setMessageHandler("ResponseMessageInter", (message: Message) => {
    //Ignore warning, handled by fragment in logic
  })

}
class Simulation {
  
}
