package old
package SimLib

import old.Commodities.Commodity
import simulation.{MarketRequest, MarketSellMessage}


class Source(commodity: Commodity, units: Int, p: Int) extends SimO with SimpleSim {
  {
    //shared.market(commodity).add_seller(this);
    make(commodity, units, 0); // at no cost
    // Put on market

  }

  override def mycopy():Source = {
    val n = new Source(commodity, units, p)
    copy_state_to(n)
    n
  }

  var initC = 0
  def action = __do{
    if (initC == 0) {
      sendMessage(MarketRequest(this.id, _root_.simulation.ENVIRONMENT_ID, commodity))
      initC = 1
    } else if (initC == 1) {
      if (markets.get(commodity).isDefined) {
        sendMessage(MarketSellMessage(this.id, markets(commodity), commodity, units, 0))
        initC = 2
      }
    } else {

    }
  }
  override def price(dummy: Commodity) = Some(p)
}


/*case class Trader(commodity: Commodity,
                  desired_inventory: Int) extends SimO with SimpleSim {
  {
    shared.market(commodity).add_seller(this);
  }

  override def mycopy():Trader = {
    val n = Trader(commodity, desired_inventory)
    copy_state_to(n)
    n
  }

  override def price(dummy: Commodity) = {
    if(available(commodity) > 0)
      Some(1.05 * inventory_avg_cost.getOrElse(commodity, 0.0))
    else None
  }

  def action = __do {
      if(available(commodity) < desired_inventory) {
        val missing = shared.market(commodity).
                        market_buy_order_now(shared.timer, this, 1);
      }
    }
}

*/

// A regular buyer for a sandbox simulation
/*
case class Buyer(commodity: Commodity,
                 units_per_tick: () => Int) extends SimO() with SimpleSim {

  override def mycopy():Buyer = {
    val n = Buyer(commodity, units_per_tick)
    copy_state_to(n)
    n
  }

  def action = __do {
      shared.market(commodity).
        market_buy_order_now(shared.timer, this, units_per_tick());
    }
}
*/


class Farm extends Factory(
  ProductionLineSpec(1, List((Commodities.Land, 1)), List(), (Commodities.Wheat, 20), 4))

class Mill extends Factory(
  ProductionLineSpec(1, List(), List((Commodities.Wheat, 10)), (Commodities.Flour, 10), 1))

class Cinema extends Factory(
  ProductionLineSpec(2, List(), List(), (Commodities.MovieTicket, 2000), 1))

class CattleFarm extends Factory(
  ProductionLineSpec(1, List((Commodities.Land, 1)), List(), (Commodities.Beef, 5), 6))

class McDonalds extends Factory(
  ProductionLineSpec(1, List(), List((Commodities.Flour, 10), (Commodities.Beef, 5)),
                 (Commodities.Burger, 10), 2))



