package object Owner {

  import Securities._

  type ITEM_T = Security

}


package Owner {

  import java.awt.TrayIcon.MessageType

  import Commodities.Commodity
  import Simulation.{AgentId, Generator, MarketBuyOrderMessage, MarketBuySellerMessage, MarketResponse, Message, TransferMoneyMessage}


  case class BalanceSheet(
                           balance: Int,
                           assets: Int,
                           capital: Int,
                           liabilities: Int,
                           short_positions: Int,
                           total_value_destroyed: Int
                         )


  /*
  class Account(
    holder      : Owner,
    _with       : Owner,
    var balance : Int = 0
  ) {

    def value(me: Owner) = {
      if(me == holder) balance
      else if(me == _with) -balance
      else { assert(false); 0 }
    }
  }


  class Handle[T](id : ID_T, table : collection.mutable.Map[ID_T, T]) {

    def get : T = table(id).asInstanceOf[T]
  }


  class CashVirtualization {
    var accounts = List[Account]()

    def capital = accounts.map(_.value)
  }
  */


  /** A legal person who manages its own finances and owns capital and inventory.
    */
  class Owner() extends Serializable {
    /** It is safe to directly modify this.
      * Changes the balance, of course.
      */
    var capital: Int = 0 // EUR cents

    var id: AgentId = Generator.getNextAgentId


    //  var accounts = List[Account]()

    protected var inventory: collection.mutable.Map[ITEM_T, Int] =
      collection.mutable.Map[ITEM_T, Int]()
    // name -> # units
    protected var inventory_avg_cost: collection.mutable.Map[ITEM_T, Double] =
      collection.mutable.Map[ITEM_T, Double]()
    private var total_value_destroyed: Double = 0.0

    /** The probability of bankruptcy, as a basis of a credit rating.
      * TODO: In which time frame?
      */
    var probfail: Double = 0.0

    protected def copy_state_to(_to: Owner) {
      //println("Owner.copy_state_to: " + this);
      _to.capital = capital;
      _to.inventory = inventory.clone();
      _to.inventory_avg_cost = inventory_avg_cost.clone();
      _to.total_value_destroyed = total_value_destroyed;
      _to.probfail = probfail;
      // TODO: reset id if last id in generator (For not creating to much ids)
      _to.id = id
    }

    //  override def toString = "(" + capital/100 + " " +
    //    inventory_to_string() + " " + total_value_destroyed + " " + probfail + ")"

    private def inventory_total_cost(item: ITEM_T): Double =
      inventory(item) * inventory_avg_cost(item)

    /** This method is to be called *before* the inventory is updated. */
    protected def recalculate_inv_avg_cost(item: ITEM_T,
                                         units_added: Int, unit_cost: Double) {
      if (!inventory.contains(item)) init_inv(item);
      if (inventory(item) + units_added == 0)
        inventory_avg_cost(item) = 0
      else
        inventory_avg_cost(item) = (inventory_total_cost(item) +
          units_added * unit_cost) / (inventory(item) + units_added)

      println("recalculate_inv_avg_cost: " + units_added + " " + unit_cost +
        " " + inventory_avg_cost(item));
    }

    /** This is the number of units in the inventory available for taking;
      * if the position is shorted (negative), available returns 0.
      */
    def available(item: ITEM_T) = math.max(0, inventory.getOrElse(item, 0))

    /** The format of each entry in the inventory is
      * `item_name -> units@cost`.
      */
    def inventory_to_string(): String = {
      inventory.map(t => t._1 + " -> " + t._2 +
        "@" + (inventory_avg_cost(t._1) / 100).toInt).toString
    }

    /**
      * This assets calculation based on costs may be wishful thinking but
      * at least it keeps the balance sheet clean.
      */
    protected def inventory_value: Double =
      (for ((item, units) <- inventory if units > 0) yield
        units * inventory_avg_cost(item)).sum

    protected def inventory_liabilities: Double =
      (for ((item, units) <- inventory if units < 0) yield
        units * inventory_avg_cost(item)).sum

    protected def assets: Double = math.max(0, capital) + inventory_value

    protected def liabilities: Double =
      math.min(0, capital) + inventory_liabilities

    def balance_sheet = BalanceSheet(
      (assets + liabilities).toInt / 100,
      math.max(0, capital / 100),
      inventory_value.toInt / 100,
      math.min(0, capital).toInt / 100,
      inventory_liabilities.toInt / 100,
      (total_value_destroyed / 100).toInt);

    /** Prints status info (balance sheet and inventory). */
    def stat {
      println((balance_sheet.toString, inventory_to_string()))
    }

    final protected def init_inv(item: ITEM_T) {
      inventory += (item -> 0);
      inventory_avg_cost += (item -> 0);
    }

    def transfer_money_to(to: AgentId, amount: Int) {
      sendMessage(TransferMoneyMessage(this.id, to, amount))
      //to.capital += amount
      capital -= amount
    }

    /** Sell strictly number of units, short selling possible. */
    /*def atomic_sell_to(buyer: Owner, item: ITEM_T, units: Int,
                       unit_price: Double) {
      assert(units >= 0); // respect trading direction: it's a sell

      if (!inventory.contains(item)) init_inv(item);

      if (this == buyer)
        println("WARNING Owner.atomic_sell_to: " + this + " selling to himself!");
      //assert(this != buyer);
      // it's robust under selling to oneself, but such a sell is probably
      // a bug elsewhere.

      if (!GLOBAL.silent)
        println((this + " sells " + units + "*" + item + " to " + buyer +
          " at " + (unit_price / 100).toInt) + "/unit");

      if (unit_price < inventory_avg_cost(item))
        println("WARNING: " + this + " is selling at a loss!");

      buyer.recalculate_inv_avg_cost(item, units, unit_price);
      recalculate_inv_avg_cost(item, -units, unit_price);

      // transfer asset
      buyer.inventory(item) += units;
      inventory(item) -= units;

      buyer.transfer_money_to(this, math.ceil(units * unit_price).toInt);

      // println("Now buyer = " + buyer + " and seller = " + this);
    }

    /** No shorting: sell no more than inventory. */
    def partial_sell_to(buyer: Owner, item: ITEM_T, units: Int,
                        unit_price: Double): Int = {
      val available = math.max(inventory(item), 0);
      val n = math.min(available, units); // no shorting

      atomic_sell_to(buyer, item, n, unit_price);

      n // return #units sold
    }*/

    /**
      * Doesn't touch capital:
      * assumes cost is already accounted for (paid for earlier).
      */
    final def make(item: ITEM_T, units: Int, unit_cost: Double) {
      assert(units > 0);
      if (!inventory.contains(item)) init_inv(item);
      recalculate_inv_avg_cost(item, units, unit_cost);


      inventory(item) += units;

    }

    /**
      * Consumes items, which get removed from the inventory and their
      * cost gets added to total_value_destroyed.
      */
    final def destroy(item: ITEM_T, units: Int): Double = {
      if (!inventory.contains(item)) init_inv(item);
      val value_destroyed = inventory_avg_cost(item) * units;
      total_value_destroyed += value_destroyed;
      inventory(item) -= units;

      value_destroyed // returns cost of destroyed stuff
    }


    // Message management

    type MESSAGE_TYPE = String
    /**
      * Contains the received messages from the previous step
      */
    protected var receivedMessages: List[Message] = List()

    /**
      * Contains the messages transfered to the other agents in the next step
      */
    protected var sendMessages: List[Message] = List()

    /**
      * Contains a map of message listeners
      */
    protected var messageListener: collection.mutable.Map[MESSAGE_TYPE, Message => Unit] = collection.mutable.Map()
    protected var responseListener: collection.mutable.Map[String, Message => Unit] = collection.mutable.Map()

    /**
      * Sets the messages from the previous step to the agent
      *
      * @param messages Actions with receiver matching the agent from the previous step
      */
    final def setReceiveMessages(messages: List[Message]): Owner = {
      this.receivedMessages = messages
      this.sendMessages = List()
      this
    }

    /**
      * Adds a list of messages to the agent
      *
      * @param messages Actions with receiver matching the agent from the previous step
      */
    final def addReceiveMessages(messages: List[Message]): Owner = {
      this.receivedMessages = messages ::: this.receivedMessages
      this
    }

    /**
      * Adds one message to the sendActions list, which will be collected and distributed at the end of the step
      *
      * @param message Action, which should be sent to a different Agent
      */
    final def sendMessage(message: Message): Unit = {
      sendMessages = message :: sendMessages
    }

    final def getMessages: List[Message] = sendMessages

    /**
      * Sets a message handler for a certain type of message
      *
      * @param messageType name of message class you want to listen for
      * @param handler     function, which handles the message
      */
    final def setMessageHandler(messageType: MESSAGE_TYPE, handler: Message => Unit): Unit = {
      messageListener += (messageType -> handler)
    }

    /**
      * Sets a message response handler for a specific session id
      *
      * @param sessionId session of message you want to listen for a response
      * @param handler     function, which handles the message
      */
    final def setMessageResponseHandler(sessionId: String, handler: Message => Unit): Unit = {
      responseListener += (sessionId -> handler)
    }

    /**
      * Logic for processing received messages.
      */
    final def handleMessages(): Owner = {
      for (msg <- receivedMessages) {
        var handler = responseListener.get(msg.sessionId)
        if (handler.isEmpty) {
          handler = messageListener.get(msg.getClass.getSimpleName)
        } else {
          responseListener.remove(msg.sessionId)
        }
        if (handler.isDefined) {
          val f = handler.get
          f(msg)
        } else {
          if (!GLOBAL.silent) {
            println("MESSAGE RECEIVED BUT COULD NOT BE HANDLED" + msg.getClass.getSimpleName + "/" + this.getClass.getSimpleName)
          }
        }
      }
      this
    }

    setMessageHandler("TransferMoneyMessage", (m:Message) => {
      val mCast = m.asInstanceOf[TransferMoneyMessage]
      this.capital += mCast.amount
    })

    setMessageHandler("MarketBuyOrderMessage", (m:Message) => {
      val mCast = m.asInstanceOf[MarketBuyOrderMessage]
      recalculate_inv_avg_cost(mCast.item, mCast.units, mCast.price)
      inventory(mCast.item) += mCast.units


      transfer_money_to(mCast.seller, math.ceil(mCast.units * mCast.price).toInt)
    })

    var markets:collection.mutable.Map[Commodity, AgentId] = collection.mutable.Map[Commodity, AgentId]()
    setMessageHandler("MarketResponse", (m: Message) => {
      val mCast = m.asInstanceOf[MarketResponse]
      markets(mCast.item) = mCast.marketId
    })
  }


} // end package Owner
