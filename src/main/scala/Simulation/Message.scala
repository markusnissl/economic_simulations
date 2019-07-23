package Simulation

abstract class Message {
  val senderId:AgentId
  val receiverId:AgentId
}

case class MarketBuyOrderMessage(override val senderId:AgentId, override val receiverId:AgentId, units: Int) extends Message
case class MarketSellMessage(override val senderId:AgentId, override val receiverId:AgentId, units: Int, price:Double) extends Message

