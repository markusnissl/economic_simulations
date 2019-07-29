package Simulation.Factory

import code._
import Owner._
import Commodities._
import Simulation.{JobFireMessage, JobHireMessage, JobHiredMessage, MarketBuyMessage, MarketRequest, MarketSellMessage, Message, ReferencePerson, ResponseMarketData, Sim, SimO}
import Timeseries.Timeseries


case class ProductionLineSpec(employees_needed: Int,
                              required: List[(Commodity, Int)],
                              consumed: List[(Commodity, Int)],
                              produced: (Commodity, Int),
                              time_to_complete: Int) {

  def theoretical_max_productivity(): Double =
    produced._2.toDouble / time_to_complete
}


// does not do its own buying
case class ProductionLine(pls: ProductionLineSpec,
                          var o: Seller,
                          salary: Int,
                          start_time: Int,

                          //  var log : List[(Int, Double)] = List(),
                          // (time production run was completed, efficiency of production run)

                          // state of the machine (this plus env)
                          var goodwill: Double = 0.0,
                          var lost_runs_cost: Double = 0.0,
                          // cost from zero-efficiency production runs
                          private var rpt: Int = 0,
                          private var frac: Double = 1.0,
                          // fraction of theoretical capacity currently achieved
                          private var costs_consumables: Double = 0.0 // of current production run

                         ) extends Sim with Serializable {


  def init(): Unit = {
    init(this.start_time)
  }

  def mycopy(_o: Seller): ProductionLine = {
    val p = this.copy()
    p.o = _o
    this.copy_state_to(p)
    //println("ProductionLine.mycopy(" + _o + "): pl.get_time = " + p.get_time);
    p
  }

  protected def algo = __forever(
    __do { // start of production run
      costs_consumables = 0
      //print("buying consumables: " + o + " " + this + ". ");
      frac = 1.0
      for (x <- pls.consumed) {
        val n = math.min(o.available(x._1), x._2); // requested and available
        costs_consumables += o.destroy(x._1, n)
        frac = math.min(frac, n.toDouble / x._2)
      }
      goodwill = costs_consumables
      if ((frac < 1.0) && (!GLOBAL.silent))
        println(o + " " + " starts low-efficiency run.")

      rpt = 0
    },
    __dowhile(
      __wait(1),
      __do {
        //print("paying salaries. ");
        // salaries are paid globally (by the factory)
        goodwill += pls.employees_needed * salary
        rpt += 1
      }
    )({
      rpt < pls.time_to_complete
    }),
    __do {
      //print("production complete! ");
      val units_produced = (pls.produced._2 * frac).toInt
      val personnel_costs = pls.employees_needed * salary *
        pls.time_to_complete
      val total_cost: Double = costs_consumables + personnel_costs
      val unit_cost = total_cost / units_produced

      if (units_produced > 0) {
        o.make(pls.produced._1, units_produced, unit_cost)

        o.sendMessage(MarketSellMessage(o.id, o.markets(pls.produced._1), pls.produced._1, units_produced, o.price(pls.produced._1).get))

        if (!GLOBAL.silent)
          println(o + " produces " + units_produced + "x " +
            pls.produced._1 + " at efficiency " + frac +
            " and " + (unit_cost / 100).toInt + "/unit.")
      }
      else {
        lost_runs_cost += total_cost

        if (!GLOBAL.silent)
          println(o + " had a production line with zero efficiency.")
      }
      //      log = (get_time, frac) :: log;
    }
  )
}


// TODO: different capabilities and salaries per employee
case class HR(private val o: Owner,
              salary: Int = 20000 * 100, // 20kEUR
              employees: collection.mutable.Stack[ReferencePerson] = collection.mutable.Stack[ReferencePerson]()
             ) {

  def pay_workers() {
    for (a <- employees) o.transfer_money_to(a.id, salary)
  }

  def salary_cost():Int = salary * employees.length

  protected def hire_one() {
    o.sendMessage(JobHireMessage(o.id, _root_.Simulation.ENVIRONMENT_ID))
    /*if (shared.arbeitsmarkt.length > 0)
      employees.push(shared.arbeitsmarkt.pop.asInstanceOf[Person]);*/
  }

  protected def fire_one() {
    o.sendMessage(JobFireMessage(o.id, _root_.Simulation.ENVIRONMENT_ID, employees.pop.id))
    //shared.arbeitsmarkt.push(employees.pop);
  }

  def hire(n: Int) {
    for (i <- 1 to n) hire_one()
  }

  def fire(n: Int) {
    for (i <- 1 to n) fire_one()
  }
}


class Factory(pls: ProductionLineSpec) extends SimO() {

  var pl: List[ProductionLine] = List()
  private var zombie_cost2: Double = 0.0 // cost from canceled prod. runs
  var prev_mgmt_action: Int = 0
  protected var hr: HR = HR(this)
  protected var goal_num_pl = 0

  // constructor
  {
    //shared.market(pls.produced._1).add_seller(this);
    goal_num_pl = 1; // have one production line
  }

  /*protected def copy_state_to(_to: Factory) {
    assert(false)
  } // don't call*/

  protected def copy_state_to(_to: Factory) {

    //println("Factory.copy_state_to: " + this);
    super.copy_state_to(_to)

    _to.pl = pl.map(_.mycopy(_to))
    _to.zombie_cost2 = zombie_cost2
    _to.prev_mgmt_action = prev_mgmt_action
    _to.hr = new HR(_to, hr.salary, hr.employees.clone()
      //hr.employees.map(old2new(_).asInstanceOf[Person])
    )
    _to.goal_num_pl = goal_num_pl
  }

  override def mycopy(): Factory = {
    val f = new Factory(pls)
    copy_state_to(f)
    f
  }


  /** Returns whether everything was sucessfully bought. */
  protected def bulk_buy_missing(_l: List[(Commodity, Int)],
                                 multiplier: Int): Boolean = {
    val l = _l.map(t => {
      // DANGER: if we have shorted his position, this amount is
      // not sufficient.
      val amount = math.max(0, t._2 * multiplier - available(t._1))
      (t._1, amount)
    });

    /*def successfully_bought(line: (Commodity, Int)) =
      (shared.market(line._1).
        market_buy_order_now(shared.timer, this, line._2) == 0);*/
    // nothing missing

    // TODO: fix me: this logic is not equivalent anymore
    def successfully_bought(line: (Commodity, Int)):Boolean = {
      if (line._2 <= 0) {
        true
      } else {
        val mId = markets.get(line._1)
        if (mId.isDefined) {
          sendMessage(MarketBuyMessage(this.id, mId.get, line._1, line._2))
        }
        false
      }
    }

    l.forall(successfully_bought)
  }

  var newEmployees = 0

  protected def add_production_line(): Boolean = {
    var success = true;

    // TODO: fix me: this logic is not equivalent anymore
    //if (shared.arbeitsmarkt.length >= pls.employees_needed) {
    if(true) {
      // buy only what we require. We may still have it from
      // previous production reductions.
      success = bulk_buy_missing(pls.required, pl.length + 1)
      if (success) {
        if (newEmployees > pls.employees_needed) {
          newEmployees -= pls.employees_needed
          val tmpPl = ProductionLine(pls, this, hr.salary, current_time)
          tmpPl.init()
          pl = tmpPl :: pl
        } else {
          hr.hire(pls.employees_needed)
        }
        //pl.head.init(shared.timer);
      }
    }
    else success = false;
    success
  }

  // We don't sell required items (land, etc.) but only fire people.
  protected def remove_production_line() {
    if (pl.nonEmpty) {
      hr.fire(pls.employees_needed)
      zombie_cost2 += pl.head.goodwill
      pl = pl.tail
    }
  }

  protected def goodwill: Int = pl.map(_.goodwill).sum.toInt

  override def stat {
    val zombie_cost = pl.map(_.lost_runs_cost).sum.toInt

    println((
      (assets + goodwill + liabilities) / 100,
      ((assets + goodwill) / 100, (assets / 100, goodwill / 100),
        liabilities / 100),
      zombie_cost.toInt / 100,
      zombie_cost2.toInt / 100,
      inventory_to_string(),
      pl.length
    ))
  }

  protected def tactics1(): Unit = {
    //Request market data
    //shared.market(pls.produced._1).order_history.toTimeseries
  }

  var marketData: Timeseries[List[SalesRecord]] = null
  var historyFetched = false

  setMessageHandler("ResponseMarketData", (m:Message) => {
    val mCast = m.asInstanceOf[ResponseMarketData]
    marketData = mCast.timeseries
    historyFetched = true
  })

  protected def tactics() = {
    import Timeseries._;

    /* cost and price concerns disregarded -- everyone makes market orders.
       supply of consumables disregarded because we want some stability
       in the simulation.

    //val labor_cost_fc = new Timeseries(shared.timer, 1.0/0, (t: Int) => salary)

    // absolute volume that will be available to us
    val sourcing_volume: Forecast = ...

    // not needed now, since everybody makes market orders
    //val sourcing_unit_cost: Int => Forecast = ... // given volume to be bought
    // for the same reason, no price forecast is needed now.
    */


    historyFetched = false
    def historic_demand: Timeseries[Int] = sum_grp[SalesRecord](marketData, _.num_ordered);

    val past_demand: Timeseries[Int] =
      historic_demand.end_at(current_time - 1);

    val demand_fc: Double =
      super_forecast(past_demand).apply(current_time - 1);


    // the number of units we can sell per tick at a price point
    //val demand_fc2: Double => Forecast = ...

    // TODO: should be big enough to be a buffer against fluctuations
    // of demand. probabilistic modeling once volatility of demand is
    // modeled.
    // val desired_inventory = 2 * demand_fc;

    // TODO: subtract the supply by other suppliers that is actually
    // traded from demand_fc.

    /*
    // the number of units we are producing if all production lines
    // are perfectly efficient (= we can source all consumables)
    def units_produced(num_pl: Int) =
      num_pl * pls.theoretical_max_productivity 

    val suitable_num_pl = argmin(1, num_pl,
          (n: Int) => math.abs(demand_fc - units_produced(n)));
    */

    // TODO: maximize assets - liabilities, not production we can sell.
    /*
      if we run production lines at low efficiency, we lose money.

      take into account how much we can sell at a price point.

      maintaining debt causes us to pay interest.


      val v = smoothe(new Timeseries(delta (assets - liabilities)), window);
      val fc = forecast(v).Apply(shared.timer + 12);
      probfail = prob(fc < 0)
    */
    /*
        // Also reduce production if we are at over capacity with respect to the
        // available consumables.
        val efficiencies = pl.flatMap(x => x.log.take(2)).map(_._2);
        val pl_fail: Boolean = (efficiencies.length - efficiencies.sum >= 2);

        if(pl_fail)
        {
          remove_production_line();
          prev_mgmt_action = shared.timer;
        }
    */

    val suitable_num_pl =
      math.ceil(demand_fc / pls.theoretical_max_productivity).toInt;

    if (!GLOBAL.silent)
      println(this + " demand_fc = " + demand_fc + ", best_pl = " +
        suitable_num_pl + ", currently = " + pl.length);


    // Do not make a sub simulation at the moment
    /*if (suitable_num_pl >= 1) {
      println(this + ": First nested simulation starts.");
      goal_num_pl = suitable_num_pl;
      val old2new1 = shared.run_sim(10);
      val future_self1 = old2new1(this).asInstanceOf[Factory];
      future_self1.stat;
      println(this + ": First nested simulation ends.");

      println(this + ": Second nested simulation starts.");
      //run simulation to see whether this is better.
      goal_num_pl -= 1;
      val old2new2 = shared.run_sim(10);
      val future_self2 = old2new2(this).asInstanceOf[Factory];
      future_self2.stat;
      goal_num_pl += 1;
      println(this + ": Second nested simulation ends.");

      def valuation(f: Factory) = f.assets + f.goodwill + f.liabilities;

      if (valuation(future_self1) < valuation(future_self2))
        println("ONE LESS WOULD BE BETTER");
      else println(goal_num_pl + " IS A GOOD DECISION!")
    }*/

    goal_num_pl = suitable_num_pl;
  }


  // This is the cost-based price of product on stock
  override def price(dummy: Commodity): Option[Double] = {
    if (available(pls.produced._1) > 0)
      Some(1.0 * inventory_avg_cost.getOrElse(pls.produced._1, 0.0))
    else None
  }

  var initC = true

  override protected def algo = __forever(
    __if(initC)(
      __do {
        // Get market ids from environment
        for (x <- pls.required) {
          sendMessage(MarketRequest(this.id, _root_.Simulation.ENVIRONMENT_ID, x._1))
        }
        for (x <- pls.consumed) {
          sendMessage(MarketRequest(this.id, _root_.Simulation.ENVIRONMENT_ID, x._1))
        }
        sendMessage(MarketRequest(this.id, _root_.Simulation.ENVIRONMENT_ID, pls.produced._1))
        initC = false
      }
    ),
    __do {

      val mgmt_step_size = 6;

      if (prev_mgmt_action + mgmt_step_size < current_time)
      //if(shared.timer % mgmt_step_size == mgmt_step_size - 1)
      {
        prev_mgmt_action = current_time; // call before tactics to avoid
        // immediate recursion in nested simulation.
        tactics1();
      }

      if (historyFetched) {
        tactics()// changes goal_num_pl
      }

      for (i <- (pl.length + 1) to goal_num_pl)
        add_production_line();
      for (i <- (goal_num_pl + 1) to pl.length)
        remove_production_line();

      // TODO: buy more to get better prices?
      //println("Factory.algo: this=" + this);
      val still_missing = bulk_buy_missing(pls.consumed, pl.length);
    },
    __wait(1),
    __do {
      //assert(hr.employees.length == pl.length * pls.employees_needed);
      hr.pay_workers();
    }
  )

  override def run_until(until: Int): (Sim, Option[Int]) = {
    // this ordering is important, so that bulk buying
    // happens before consumption.
    val nxt1 = super.run_until(until)._2.get
    if (pl.nonEmpty) {
      val nxt2 = pl.map(_.run_until(until)._2.get).min
      (this, Some(math.min(nxt1, nxt2))) // compute a meaningful next time
    } else {
      (this, Some(nxt1))
    }
  }


  setMessageHandler("JobHiredMessage", (m: Message) => {
    val mCast = m.asInstanceOf[JobHiredMessage]
    hr.employees.push(new ReferencePerson(this, mCast.employeeId))
    newEmployees += 1
  })

}


