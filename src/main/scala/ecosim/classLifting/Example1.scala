package ecosim.classLifting
import squid.quasi._
import ecosim.runtime.Actor

case class Other() {
  def met2(par1: String) = {
    println(par1)
  }
}

@lift
class Actor1 extends Actor {
  val a = List(1,2,3)
  val b = List(2,3,4)
  var c = 10
  var g = "string"
  var other = Other()
  //def met2(a: Int) = println("whoa")
  def met1(par1: Int, par2: Int) = {
//    met2(2)
    a.foreach(x => b.foreach(y => println(x + y)))
    var d = 0
    while (true) {
      a.foreach(var1 => b.foreach(var2 => {
        d = var1 + var2
        println(d)
      }))
    }
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

