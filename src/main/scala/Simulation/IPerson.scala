package Simulation

import Owner.Owner
import code.{__forever, __syncMessage, __wait}




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

  protected def algo = __forever(
    rPerson.sell(10+this.id.toInt,10+this.id.toInt),
    __wait(1)
  )

  setMessageHandler("RequestMessage", (message: Message) => {
    val mCast:RequestMessage = message.asInstanceOf[RequestMessage]
    val result = mCast.call_f(this)
    mCast.reply(this, result)
  })
}

class RPerson(owner: Owner, id: AgentId) extends IPerson with Serializable {
  case class sell(unit: Int, price: Double) extends __syncMessage(
    owner,
    () => id,
    (result: Any) => {
      println(result)
      println(result.asInstanceOf[(Boolean, Boolean)]._1)
    },
    x => x.asInstanceOf[TPerson].sell(unit, price)()
  )
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