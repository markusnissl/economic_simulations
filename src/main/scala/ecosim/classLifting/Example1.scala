package ecosim.classLifting
import simulation.core.Actor
import squid.quasi._

//TODO: make a new annotation for main method?
@lift
class MainClass {
  def main(): List[Actor] = {
    val a = new Actor1()
    List(a, a.actor2)
  }
}

@lift
class Actor2 extends Actor {
  def met2(par1: String) = {
    println(par1)
  }
  def sell(number: Int, price: Int)(msg: String): NBUnit = {println(msg); NBUnit()}
  def main() = {
    while (true){
      sell(1,22)("i sell stuff")
      SpecialInstructions.waitTurns(1)
    }
  }
}

@lift
class Actor1() extends Actor {
  val a = List(1,2,3)
  val b = List(2,3,4)
  var c = 10
  var g = "string"
  var actor2 = new Actor2()
  def main() = {
    while(true) {
      met1(1,65)(3)
      met3()
//      actor2.sell(5,3)("I buy stuff")
      SpecialInstructions.waitTurns(3)
    }
  }
  def met3(): String = {actor2.met2("afk"); "afk"}
  def met1(par1: Int, par2: Int)(par3: Int) = {
    println("whoa")
    3
  }
}

