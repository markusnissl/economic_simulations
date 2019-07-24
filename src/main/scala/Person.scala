package Simulation

import code._
import Commodities._


class Person(
              val active: Boolean,
              var happiness: Int = 0, // pursuit of it
              var log: List[String] = List()
            ) extends SimO {

  override def mycopy(): Person = {
    val p = new Person(active, happiness, log)
    copy_state_to(p)
    p
  }

  private val properties: Map[Commodity, Map[String, Int]] =
    Map(Flour -> Map("calories" -> 100),
      Burger -> Map("calories" -> 500))

  private val foodstuffs = List(Flour, Burger)

  // TODO: factor in bounded rationality: far-off rewards are to be discounted
  private def expected_enjoyment(item: Commodity): Int = {
    item match {
      case MovieTicket => 1
      case _ if properties(item).contains("calories") =>
        properties(item)("calories")
      case _ => 0
    }
  }

  private def consume(consumable: Commodity, units: Int) {
    assert(available(consumable) >= units)
    happiness += units * expected_enjoyment(consumable)
    destroy(consumable, units)
    log = (units + "*" + consumable + "@" + current_time) :: log
  }

  var init = false
  protected def algo = __forever(
    __if(init)(
      __do {
        init = true
        sendMessage(MarketRequest(this.id, ENVIRONMENT_ID, Flour))
        sendMessage(MarketRequest(this.id, ENVIRONMENT_ID, Burger))
        sendMessage(MarketRequest(this.id, ENVIRONMENT_ID, MovieTicket))
      }
    ),
    __do {
      if (active) {
        val food = if (GLOBAL.rnd.nextInt(2) == 0) Flour else Burger

        happiness -= 100; // hunger

        // assert(market(food).is_at_time(shared.timer));
        val fmId = markets.get(food)
        if (fmId.isDefined) {
          sendMessage(MarketBuyMessage(this.id, fmId.get, food, 1))
        }
        //shared.market(food).market_buy_order_now(shared.timer, this, 1)
        // needs to eat
        if (available(food) >= 1) consume(food, 1)

        val mtmId = markets.get(MovieTicket)
        if (mtmId.isDefined) {
          sendMessage(MarketBuyMessage(this.id, mtmId.get, MovieTicket, 1))
        }
        //shared.market(MovieTicket).market_buy_order_now(shared.timer, this, 1)
        // wants entertainment
        if (available(MovieTicket) >= 1) consume(MovieTicket, 1)

        // shared.market("miete").market_buy_order_now(shared.timer, this, 1);
      }
    },
    __wait(1)
  )

  override def stat {
    print("(Person@" + happiness + " " + capital / 100 + ")  ")
  }
}


