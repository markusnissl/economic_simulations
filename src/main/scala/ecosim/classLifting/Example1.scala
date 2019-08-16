package ecosim.classLifting
import squid.quasi._
import ecosim.runtime.Actor

//TODO: make a new annotation for main method?
@lift
class MainClass {
  def mainLoop(): List[Actor] = {
    val a = new Actor1()
    List(new Actor2(), a)
  }
}

@lift
class Actor2 extends Actor {
  def met2(par1: String) = {
    println(par1)
  }
  def sell() = println("i sell stuff")
  def main() = {
    while(true){
      sell()
      SpecialOperations.waitTurns(1)
    }
  }
}

@lift
class Actor1 extends Actor {
  val a = List(1,2,3)
  val b = List(2,3,4)
  var c = 10
  var g = "string"
  val map = collection.mutable.Map[Int,Int]()
  var actor2 = new Actor2()
  def main() = {
    while(true) {
      actor2.sell()
      SpecialOperations.waitTurns(3)
    }
  }
  def met2(figure: Int) = if (3 == 3) println("PRINTING 3")
  def met3(): String = {actor2.met2("afk"); "afk"}
  def met1(par1: Int, par2: Int) = {
    println("whoa")
    3

//    a.foreach(x => b.foreach(y => println(x + y)))
//    var d = 0
//    while (true) {
//      /a.foreach(var1 => b.foreach(var2 => {
//        d = var1 + var2
//        println(d)
//      }))
//    }
//    while (true) {
//      a.foreach(x => println(x))
//    }
//    while (true) {
//      while (true) {
//        println("text")
//      }
//    }
//    println('a');println(42);123
  }
}

