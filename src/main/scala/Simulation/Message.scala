package Simulation

import java.util.UUID

import Commodities.Commodity
import Owner.{ITEM_T, Owner, SalesRecord}
import Timeseries.Timeseries
import ecosim.deep
import ecosim.deep.{IR, NonLocalMethod}


abstract class Message extends Serializable {
  val senderId: AgentId
  val receiverId: AgentId
  var sessionId: String = UUID.randomUUID().toString

  override def toString: String = {
    return "Message: " + senderId + " -> " + receiverId + "(" + sessionId + ")"
  }
}

//Market Management
case class MarketRequest(override val senderId: AgentId, override val receiverId: AgentId, item: Commodity) extends Message

case class MarketResponse(override val senderId: AgentId, override val receiverId: AgentId, item: Commodity, marketId: AgentId) extends Message {
  def this(message: MarketRequest, marketId: AgentId) {
    this(message.receiverId, message.senderId, message.item, marketId)
    sessionId = message.sessionId
  }
}

case class MarketBuyMessage(override val senderId: AgentId, override val receiverId: AgentId, item: ITEM_T, units: Int) extends Message

case class MarketBuyOrderMessage(override val senderId: AgentId, override val receiverId: AgentId, seller: AgentId, item: ITEM_T, units: Int, price: Double) extends Message

case class MarketBuySellerMessage(override val senderId: AgentId, override val receiverId: AgentId, buyer: AgentId, item: ITEM_T, units: Int, price: Double) extends Message

case class MarketSellMessage(override val senderId: AgentId, override val receiverId: AgentId, item: ITEM_T, units: Int, price: Double) extends Message


//Money Management
case class TransferMoneyMessage(override val senderId: AgentId, override val receiverId: AgentId, amount: Int) extends Message


//Job Management
case class JobHireMessage(override val senderId: AgentId, override val receiverId: AgentId) extends Message

case class JobHiredMessage(override val senderId: AgentId, override val receiverId: AgentId, employeeId: AgentId) extends Message

case class JobFireMessage(override val senderId: AgentId, override val receiverId: AgentId, employeeId: AgentId) extends Message


case class RequestMarketData(override val senderId: AgentId, override val receiverId: AgentId, item: ITEM_T) extends Message

case class ResponseMarketData(override val senderId: AgentId, override val receiverId: AgentId, timeseries: Timeseries[List[SalesRecord]]) extends Message

// General message
case class RequestMessageInter[A,B](override val senderId: AgentId, override val receiverId: AgentId, mtd: NonLocalMethod[A,B], arg: Any) extends Message {
  //TODO: fix me reply
  def reply(owner: Owner, returnValue:Any): Unit = {
    val msg = ResponseMessageInter(receiverId, senderId, returnValue)
    msg.sessionId = this.sessionId
    owner.sendMessage(msg)
  }
}
case class ResponseMessageInter[A,B](override val senderId: AgentId, override val receiverId: AgentId, arg: Any) extends Message

case class RequestMessage(override val senderId: AgentId, override val receiverId: AgentId, call_f: Any => Any) extends Message {

  def reply(owner: Owner, returnValue:Any): Unit = {
    val msg = ResponseMessage(receiverId, senderId, returnValue)
    msg.sessionId = this.sessionId
    owner.sendMessage(msg)
  }
}

case class ResponseMessage(override val senderId: AgentId, override val receiverId: AgentId, result: Any) extends Message {}


// DEMO for proxy
case class PersonSellRequest(override val senderId: AgentId, override val receiverId: AgentId, unit: Int, price: Int) extends Message

case class PersonSellResponse(override val senderId: AgentId, override val receiverId: AgentId) extends Message {
  def this(message: PersonSellRequest) {
    this(message.receiverId, message.senderId)
    sessionId = message.sessionId
  }
}

case class PersonNameRequest(override val senderId: AgentId, override val receiverId: AgentId) extends Message

case class PersonNameResponse(override val senderId: AgentId, override val receiverId: AgentId, name: String) extends Message {
  def this(message: PersonNameRequest, name: String) {
    this(message.receiverId, message.senderId, name)
    sessionId = message.sessionId
  }
}