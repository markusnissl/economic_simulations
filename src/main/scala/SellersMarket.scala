package Owner {

  import Timeseries._
  import Securities._
  import Commodities._
  import Simulation.{AgentId, MarketBuySellerMessage, Message}


  case class SalesRecord(
                          buyer: AgentId,
                          matching: List[(AgentId, Int, Int)], // (seller, units, unit_price)
                          num_ordered: Int,
                          num_sold: Int,
                          total_price: Int
                        ) {
    def missed_sales(at_price: Double): Int = {
      if (total_price > num_sold * at_price)
        num_ordered // we would have made a cheaper offer
      else num_ordered - num_sold
      // even though we are expensive, we make the deal
      // because it's a market order
    }
  }


  class Seller extends Owner {
    var order_history: LogList[SalesRecord] = new LogList[SalesRecord]

    protected def copy_state_to(_to: Seller) {
      //println("Seller.copy_state_to: " + this);
      super.copy_state_to(_to)
      _to.order_history = order_history.copy()
    }

    // only one global price, not for speculators.
    // override to offer stuff.
    def price(item: Commodity): Option[Double] = None

    /** A partial sell, using the price from this[Seller].price.
      * *
      * Note that this corresponds to partial_sell_to in Owner, not sell_to.
      * Writes a SalesRecord to order_history.
      * TODO: give all or nothing option.
      */
    /*def sell_to(time: Int, buyer: Owner, item: Commodity, units: Int): Int = {
      price(item) match {
        case (Some(p)) => {
          val units_sold = partial_sell_to(buyer, item, units, p);
          order_history.add(time,
            SalesRecord(buyer, List(), units, units_sold,
              (units_sold * p).toInt));
          // create entry even if nothing is sold
          units_sold // TODO: also return price (unit or total?)
        }
        case None => 0
      }
    }*/

  }


} // package Owner


package Markets {

  import Owner._
  import Timeseries._
  import Securities._
  import Commodities._
  import Simulation.{AgentId, MarketBuyMessage, MarketBuyOrderMessage, MarketBuySellerMessage, MarketSellMessage, Message, ResponseMarketData, SimO}
  import code.{Instruction, __forever, __do, __wait}


  /**
    * Not all sellers registered here may be selling at a time.
    * One can buy as much of the inventory as the seller has.
    * Each seller has one price for all its ware.
    * Orders are fulfilled immediately,
    * partially if not enough ware is on the market.
    * Orders may be fulfilled using inventories of multiple sellers.
    * Fulfillment is at best prices.
    */
  class SellersMarket(commodity: Commodity) extends SimO with MarketSelling with MarketMatchingUtilities[(AgentId, Int, Double)] {
    //var sellers = List[Seller]()
    //Seller,Units,Price
    var goods: List[(AgentId, Int, Double)] = List()

    //var order_history = new LogList[SalesRecord]

    def copy_state_to(other: SellersMarket,
                      old2new: Seller => Seller) {
      other.order_history = order_history.copy()
      //other.sellers = sellers.map(old2new(_));
    }

    override def mycopy(): SimO = {
      val n = new SellersMarket(commodity)
      copy_state_to(n)
      n
    }

    override def algo: Instruction = __forever(__do{}, __wait(1))

    /*def add_seller(s: Seller) {
      sellers = s :: sellers;
    }*/

    /** returns (#unmatched, List[(#matched with this seller, seller)]).  */
    private def best_match(units: Int,
                           exclude: AgentId): (Int, List[(Int, (AgentId, Int, Double))]) = {
      //println("best_match sellers: " + sellers);

      // lowest price first
      val asks = goods.filter(_._1 != exclude).sortBy(_._3)
      /*sellers.filter((s: Seller) =>
        (s.price(commodity).isDefined) && (s != exclude)).sorted(
        Ordering.by[Seller, Double](
          (s: Seller) => s.price(commodity).get))*/

      //greedy_match(asks, ((s: Seller) => s.available(commodity)), units)
      greedy_match(asks, x => x._2, units)
    }

    private def compute_price(l: List[(Int, (AgentId, Int, Double))]): Double =
      l.map(x => x._2._3 * x._1).sum

      /*l.map((t: (Int, Seller)) =>
      t._2.price(commodity).getOrElse(1.0 / 0) * t._1).sum*/

    def ask_price(units: Int): (Double, Int) = {
      val (left_over, l) = best_match(units, -1)
      (compute_price(l), units - left_over)
    }

    def ask_price(): Option[Double] = {
      val (p, l) = ask_price(1)
      if (l == 0) None else Some(p)
    }

    /** execute immediately, partial fulfillment possible. */
    def market_buy_order_now(time: Int, buyer: AgentId, units: Int): Int = {
      //println("SellersMarket.market_buy_order_now " + this);
      val (left_over, l) = best_match(units, buyer)
      //println("Buying " + units + " on market: " + l + " " + left_over);
      val p = compute_price(l); // can't reorder this line and the next
      for ((u, s) <- l) {
        sendMessage(MarketBuyOrderMessage(this.id, buyer, s._1, commodity, u, s._3))
        sendMessage(MarketBuySellerMessage(this.id, s._1, buyer, commodity, u, s._3))
        var x = goods.find(_._1 == s._1).get
        var y = (x._1,x._2-u,x._3)
        goods = y :: goods.filter(_._1 != s._1)
        //s.sell_to(time, buyer, commodity, u)
      }
      val sold = units - left_over

      order_history.add(time, SalesRecord(buyer, List(), units, sold, p.toInt));
      left_over
    }

    /** not implemented */
    def limit_buy_order_now(time: Int, buyer: Owner, units: Int): Int = {
      assert(false)
      0
    }

    setMessageHandler("MarketSellMessage", (m:Message) => {
      val mCast = m.asInstanceOf[MarketSellMessage]
      val seller = goods.find(_._1 == mCast.senderId)
      if (seller.isDefined) {
        val y = (mCast.senderId, seller.get._2 + mCast.units, mCast.price)
        goods = y :: goods.filter(_._1 != mCast.senderId)
      } else {
        val y = (mCast.senderId, mCast.units, mCast.price)
        goods = y :: goods
      }
    })

    setMessageHandler("MarketBuyMessage", (m:Message) => {
      val mCast = m.asInstanceOf[MarketBuyMessage]

      market_buy_order_now(current_time, mCast.senderId, mCast.units)
    })

    setMessageHandler("RequestMarketData", (m:Message) => {
      sendMessage(ResponseMarketData(this.id, m.senderId, order_history.toTimeseries))
    })
  }


} // package Markets


/*
class OwnerOnMarket extends Owner {
  // this assets calculation takes market prices into account, where available.
  def assets(market: collection.mutable.Map[Commodity, SellersMarket]) : Int = {
    var write_off = 0;
    math.max(0, capital) +
    (for((item, units) <- inventory) yield {
      val cost = units * inventory_avg_cost(item).toInt;
      market(item).ask_price() match {
        case Some(p) => {
          val market_value = (units * p).toInt
          write_off += cost - market_value;
          market_value
        }
        case None    => cost
      }
    }).sum
    // TODO: could also return math.max(0, write_off)
  }
}
*/


