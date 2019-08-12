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

  var stepFunction: (Int, Int, Int) => (Int, Int) = null
  var useStepFunction = false

  //Override with compiled function, has to be set before
  override def run_until(until: T) = {
    if (useStepFunction) {
      println("Run until", current_pos, current_time)
      val (a, b) = stepFunction(current_pos, current_time, until)
      println("Run until finished", a, b)
      current_pos = a;
      current_time = b;
      (this, Some(b + 1))
    } else {
      super.run_until(until)
    }
  }
}
class Simulation {
  
}
