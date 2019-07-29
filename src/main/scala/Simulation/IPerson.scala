package Simulation

import Owner.Owner
import code.{__do, __forever, __syncMessage, __wait}




trait IPerson {
  abstract class sell(unit: Int, price: Double) {
    def apply(): (Boolean, Boolean)
  }
}

class TPerson extends SimO with IPerson {

  println(this.id)
  override def mycopy(): TPerson = {
    val p = new TPerson()
    copy_state_to(p)
    p
  }

  case class sell(unit: Int, price: Double) {
    def apply(): (Boolean, Boolean) = {
      println(unit, price)
      (true, false)
    }
  }

  // FIXME: Test for two person a send to b and b to a
  val rPerson:RPerson = new RPerson(this, 7+this.id%2)

  val callSell = rPerson.sell(10+this.id.toInt,10+this.id.toInt)
  protected def algo = __forever(
    callSell,
    __do {
      val result = callSell()
      println("ALGO", result)
    },
    __wait(1)
  )

  setMessageHandler("RequestMessage", (message: Message) => {
    val mCast:RequestMessage = message.asInstanceOf[RequestMessage]
    val result = mCast.call_f(this)
    mCast.reply(this, result)
  })
}

class RPerson(owner: Owner, id: AgentId) extends IPerson with Serializable {
  var resultF: (Boolean, Boolean) = (false, false)

  case class sell(unit: Int, price: Double) extends __syncMessage(
    owner,
    () => id,
    (result: Any) => {
      resultF = result.asInstanceOf[(Boolean, Boolean)]
    },
    x => x.asInstanceOf[TPerson].sell(unit, price)()
  ) {
    def apply(): (Boolean, Boolean) = {
      resultF
    }
  }
}

object MessagePassingExample {
  val s = new Simulation
  s.init(List(
    new TPerson(),
    new TPerson()
  ))
  def main(argv: Array[String]) {
      s.run(10)
  }
}