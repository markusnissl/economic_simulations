package simulation

import old.Owner.Owner


//TODO: make a proxy of real person and forward message
class ReferencePerson(val owner: Owner, val id: AgentId)  extends Serializable {

  var message: PersonNameResponse = _
  var message2: PersonSellResponse = _

  def getName(f: String => Unit): (String, Message => Unit, Owner) = {
    val msg = PersonNameRequest(owner.id, id)

    def messageCallback: Message=>Unit = (m: Message) => {
      message = m.asInstanceOf[PersonNameResponse]

      f(message.name)
    }

    owner.sendMessage(msg)

    (msg.sessionId, messageCallback, owner)
  }

  def sell(unit: Int, price:Int): (String, Message => Unit, Owner) = {
    val msg = PersonSellRequest(owner.id, id, unit, price)

    def messageCallback: Message=>Unit = (m: Message) => {
      message2 = m.asInstanceOf[PersonSellResponse]

      println("Sell successfull")
    }

    owner.sendMessage(msg)

    (msg.sessionId, messageCallback, owner)
  }
}