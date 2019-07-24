package Simulation

import Commodities.Commodity
import Owner.{ITEM_T, SalesRecord}
import Timeseries.Timeseries

abstract class Message {
  val senderId: AgentId
  val receiverId: AgentId
}

//Market Management
case class MarketRequest(override val senderId: AgentId, override val receiverId: AgentId, item: Commodity) extends Message

case class MarketResponse(override val senderId: AgentId, override val receiverId: AgentId, item: Commodity, marketId: AgentId) extends Message

case class MarketBuyOrderMessage(override val senderId: AgentId, override val receiverId: AgentId, seller: AgentId, item: ITEM_T, units: Int, price: Double) extends Message

case class MarketBuySellerMessage(override val senderId: AgentId, override val receiverId: AgentId, buyer: AgentId, item: ITEM_T, units: Int, price: Double) extends Message

case class MarketSellMessage(override val senderId: AgentId, override val receiverId: AgentId, item: ITEM_T, units: Int, price: Double) extends Message

case class MarketBuyMessage(override val senderId: AgentId, override val receiverId: AgentId, item: ITEM_T, units: Int) extends Message


//Money Management
case class TransferMoneyMessage(override val senderId: AgentId, override val receiverId: AgentId, amount: Int) extends Message


//Job Management
case class JobHireMessage(override val senderId: AgentId, override val receiverId: AgentId) extends Message

case class JobHiredMessage(override val senderId: AgentId, override val receiverId: AgentId, employeeId: AgentId) extends Message

case class JobFireMessage(override val senderId: AgentId, override val receiverId: AgentId, employeeId: AgentId) extends Message


case class RequestMarketData(override val senderId: AgentId, override val receiverId: AgentId, item: ITEM_T) extends Message

case class ResponseMarketData(override val senderId: AgentId, override val receiverId: AgentId, timeseries: Timeseries[List[SalesRecord]]) extends Message