package ecosim.classLifting
import squid.quasi._
import ecosim.runtime.Actor

//TODO: make a new annotation for main method?
@lift
class MainClass {
  def mainLoop(): List[Actor] = {
    val a = new Actor1()
    List(new Actor1(), new Actor2(), new Actor1(), a)
  }
}

case class Other() {
  def met2(par1: String) = {
    println(par1)
  }
  def loop() = {
    println("A")
  }
}

@lift
class Actor2 extends Actor {
  def sell() = println("i sell stuff")
}

@lift
class Actor1 extends Actor {
  val a = List(1,2,3)
  val b = List(2,3,4)
  var c = 10
  var g = "string"
  val map = collection.mutable.Map[Int,Int]()
  var other = Other()
  def loop() = {
    while(true) {
      a.foreach(x => {
        met2(x)
      })
    }
  }
  def met2(a: Int) = if (a == 3) println(a)
  def met3(): String = {other.met2("afk"); "afk"}
  def met1(par1: Int, par2: Int) = {
    println("a")
    println("b")
    println("c")
    map(1) = 1
    3

//    a.foreach(x => b.foreach(y => println(x + y)))
//    var d = 0
//    while (true) {
//      a.foreach(var1 => b.foreach(var2 => {
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

