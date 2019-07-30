package Simulation
import code._
import Owner._


trait Sim {
  type T = Int // time type
  private val zero : T = 0

  // BEGIN state
  private var current_pos  : Int = 0
  protected var current_time : T   = zero
  // END state

  protected def algo   : Instruction
  var algo_c : Vector[SimpleInstruction] = _

  /** Call from the constructor in inheriting classes. */
  protected def init(start_time: T) {
    current_pos    = 0;
    current_time   = start_time;
    algo_c = compile(algo)
  }

  protected def copy_state_to(_to: Sim) {
    _to.current_pos    = current_pos
    _to.current_time   = current_time;
    _to.algo_c = compile(_to.algo);
  }

  /** Runs until at most time `until`. */
  def run_until(until: T) = {
    val (a, b, next_goal_time) =
      exec[T](algo_c, current_pos, current_time, until);

    current_pos = a;
    current_time = b;
    (this, next_goal_time)
  }
}


object Sim {

  def execp(sims: Seq[Sim], start_time: Int, end_time: Int) =
    code.execp[Sim, Int](sims, (s: Sim, t: Int) => s.run_until(t),
                         start_time, end_time)

} // end object Sim.


trait SimpleSim extends Sim {
  def action : Instruction
  override def algo = __forever(action, __wait(1))
}


abstract class SimO(start_time: Int = 0) extends Seller with Sim {

  def init(): Unit = {
    super[Sim].init(start_time)
  }

  protected def copy_state_to(_to: SimO) = {
    super[Seller].copy_state_to(_to);
    super[Sim].copy_state_to(_to);
  }

  def mycopy(): SimO

  // Put it here instead of seller, since there i have access to time
  setMessageHandler("MarketBuySellerMessage", (m:Message) => {
    val mCast = m.asInstanceOf[MarketBuySellerMessage]

    recalculate_inv_avg_cost(mCast.item, -mCast.units, mCast.price)

    // Reduce asset
    inventory(mCast.item) -= mCast.units

    order_history.add(current_time, SalesRecord(mCast.buyer, List(), mCast.units, mCast.units,
      (mCast.units * mCast.price).toInt));
  })
}


