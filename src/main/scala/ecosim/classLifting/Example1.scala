package ecosim.classLifting
import squid.quasi._
import ecosim.runtime.Actor

//TODO: make a new annotation for main method?
@lift
class MainClass {
  def main(): List[Actor] = {
    val b = new Actor2()
    val a = new Actor1(b)
    List(b,a)
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
      SpecialInstructions.handleMessages()
    }
  }
}

@lift
class Actor1(val actor2: Actor2) extends Actor {
  val a = List(1,2,3)
  val b = List(2,3,4)
  var c = 10
  var g = "string"
  def main() = {
    while(true) {
      if(3 == 3) println(3)
      SpecialInstructions.waitTurns(3)
    }
  }
  def met3(): String = {actor2.met2("afk"); "afk"}
  def met1(par1: Int, par2: Int)(par3: Int) = {
    println("whoa")
    3
  }
}

