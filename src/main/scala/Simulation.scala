package Simulation

import Markets._
import Owner._
import Commodities._
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}


class Simulation extends Serializable {
  var timer = 0;

  var market = collection.mutable.Map[Commodity, SellersMarket]();
  for (c <- Commodities.all_commodities) {
    market += (c -> new SellersMarket(c));
  };

  //var arbeitsmarkt = collection.mutable.Stack[SimO](); // all Persons
  var arbeitsmarkt = collection.mutable.Stack[AgentId](); // all Persons

  //var chicago = collection.mutable.Map[Security, OrderBook]();


  /** TODO: We should have a registry of sims here, which can be looked up by
    * id. This eliminates the need for substitution when copying a simulation,
    * which is a mess.
    */
  @transient var sims = List[SimO]()

  /**
    * Spark part here
    */
  @transient lazy val conf: SparkConf = new SparkConf().setMaster("local").setAppName("ECONOMIC_SIMULATION")
  @transient lazy val sc: SparkContext = new SparkContext(conf)

  @transient var simsSpark:RDD[(AgentId, SimO)] = _



  /** This is not a constructor since we first need to create the Simulation
    * to hand it over to the sims, and then hand
    * the sims to the Simulation (via init).
    * *
    * init() accepts the list of sims `_sims`,
    * enters persons into the labor market,
    * and output the status of each sim.
    */
  @transient def init(_sims: List[SimO]) {
    assert(timer == 0);
    println("INIT Simulation " + this);
    sims = _sims;

    //for(s <- sims) if(s.isInstanceOf[Person]) arbeitsmarkt.push(s);
    for (s <- sims if s.isInstanceOf[Person]) arbeitsmarkt.push(s.id);

    if (!GLOBAL.silent) {
      for (s <- sims) {
        s.stat;
      }
      println;
      println;
    }

    if (GLOBAL.RUN_SPARK) {
      sc.setLogLevel("ERROR")
      simsSpark = sc.parallelize(sims).union(sc.parallelize(market.values.toList)).map(x => (x.id,x))
    }

    println("INIT Simulation complete " + this);
  }

  /** TODO: Object ids (owners) in logs don't get substituted yet.
    * This will become necessary when we want to compute supply by _other_
    * sellers.
    */
  @transient def mycopy() = {
    val s2 = new Simulation;
    val old2new = collection.mutable.Map[SimO, SimO]();

    // this separation would not be needed if we had a central map from sim ids
    // to sims.
    /*for(s <- sims) {
      if(s.isInstanceOf[Person]) {
        val cp = s.mycopy().asInstanceOf[SimO];
        old2new += (s -> cp);
      }
    }*/

    s2.sims = sims.map((s: SimO) => {
      old2new.getOrElse(s, {
        val cp = s.mycopy().asInstanceOf[SimO];
        old2new += (s -> cp);
        cp
      })
    });

    s2.arbeitsmarkt = arbeitsmarkt.clone()

    /*s2.arbeitsmarkt =
      arbeitsmarkt.map((x: SimO) => old2new(x.asInstanceOf[SimO]));*/

    for ((commodity, ma) <- market) {
      ma.copy_state_to(s2.market(commodity),
        (s: Seller) => old2new(s.asInstanceOf[SimO]));
    }

    /*assert(s2.chicago.isEmpty);
    for ((security, ob) <- chicago) {
      s2.chicago += (security -> ob.mycopy());
    }*/

    s2.timer = timer;

    (s2, old2new)
  }

  def handleEnvMessage(message: Message): List[Message] = message match {
    case msg: MarketRequest => {
      List[Message](MarketResponse(ENVIRONMENT_ID, msg.senderId, msg.item, market(msg.item).id))
    }
    case msg: JobHireMessage => {
      if (arbeitsmarkt.nonEmpty) {
        List[Message](JobHiredMessage(ENVIRONMENT_ID, msg.senderId, arbeitsmarkt.pop()))
      } else {
        List[Message]()
      }
    }
    case msg: JobFireMessage => {
      arbeitsmarkt.push(msg.employeeId)
      List[Message]()
    }
    case x => {
      println("Illegal instruction: " + x.getClass.getSimpleName)
      List[Message]()
    }
  }


  /** run the simulation. Must init() first! */
  @transient def run_until(until: Int) {
    println("RESUME Simulation " + this);
    var messages: List[Message] = List()
    while (timer <= until) {
      if (!GLOBAL.silent) println("timer = " + timer);

      if (GLOBAL.RUN_SPARK) {
        //          s.setReceiveMessages(mx.getOrElse(s.id, List()))
        simsSpark = simsSpark.mapValues{ s =>
          s.handleMessages()
          s.run_until(timer)._1.asInstanceOf[SimO]
        }.cache()

        var dMessages:RDD[(AgentId, List[Message])] = simsSpark.flatMap(_._2.getMessages).map(x => (x.receiverId, x)).combineByKey(
          (message: Message) => {
            List(message)
          },
          (l: List[Message], message:Message) => {
            message :: l
          },
          (l1: List[Message], l2: List[Message]) => {
            l1 ::: l2
          }
        ).cache()

        // Environment answers immediatley for the next step
        val envMessages:RDD[(AgentId, List[Message])] = dMessages.filter(_._1 == ENVIRONMENT_ID).flatMap(_._2).flatMap(handleEnvMessage).map( x => (x.receiverId, x)).combineByKey(
          (message: Message) => {
            List(message)
          },
          (l: List[Message], message:Message) => {
            message :: l
          },
          (l1: List[Message], l2: List[Message]) => {
            l1 ::: l2
          }
        ).cache()

        // Append environment to messages: important merge both together (groupByKey otherwise elements may be duplicated at join afterwards)
        dMessages = dMessages.filter(_._1 != ENVIRONMENT_ID).union(envMessages).groupByKey().mapValues(_.flatten.toList)
        dMessages = dMessages.cache()

        simsSpark = simsSpark.leftOuterJoin(dMessages).mapValues{x =>
          x._1.setReceiveMessages(x._2.getOrElse(List()))
          x._1
        }.persist()


        if (!GLOBAL.silent) {
          simsSpark.foreach(_._2.stat)
          println()
          println()
        }

      } else {
        messages = messages.filter(_.receiverId == ENVIRONMENT_ID).flatMap(handleEnvMessage) ::: messages.filter(_.receiverId != ENVIRONMENT_ID)

        val mx = messages.groupBy(_.receiverId)
        messages = List()

        market = market.map(m => {
          m._2.setReceiveMessages(mx.getOrElse(m._2.id, List()))
          m._2.handleMessages()
          m
        })

        messages = market.flatMap(_._2.getMessages).toList ::: messages

        sims = sims.map {
          s =>
            s.setReceiveMessages(mx.getOrElse(s.id, List()))
            s.handleMessages()
            s.run_until(timer)._1.asInstanceOf[SimO]
        }
        messages = sims.flatMap(_.getMessages) ::: messages

        if (!GLOBAL.silent) {
          for (s <- sims) s.stat;
          println();
          println();
        }
      }

      timer += 1;
    }
    println("STOP Simulation " + this);
  }

  @transient def run(steps: Int) {
    run_until(timer + steps - 1);
  }

  /** To be used to start a nested simulation. Callable from the sims.
    * *
    * Returns a mapping from the sims of the old simulation to those of the
    * new.
    */
  @transient def run_sim(it: Int): collection.mutable.Map[SimO, SimO] = {
    val (new_sim, old2new) = this.mycopy

    // prevent recursive simulation. This is only safe it the simulation
    // runs for fewer than 1000 iterations!
    for (s <- new_sim.sims)
      if (s.isInstanceOf[Factory.Factory])
        s.asInstanceOf[Factory.Factory].prev_mgmt_action += 1000

    new_sim.run(it);
    old2new
  }
}



