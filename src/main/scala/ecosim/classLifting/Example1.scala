package ecosim.classLifting
import squid.quasi._
import ecosim.runtime.Actor

@lift
class Actor1 extends Actor {
  //val a = 5
  val a = List(1,2,3)
  val b = List(2,3,4)
  def met1(par1: Int, par2: Int) = {
    //var c = 10 //TODO check if it works with new version
    //a.foreach(x => b.foreach(y => println(x + y)))
    //while (true) {
    //  a.foreach(x => println(x))
    //}
//    while (true) {
//      while (true) {
//        println("bro")
//      }
//    }
    //println('a');println(42);123
  }
}

