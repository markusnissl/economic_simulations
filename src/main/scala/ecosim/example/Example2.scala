package ecosim.example.ex2

/* Lifting test, comment in if needed or if package is available
import squid.ir.{ClassEmbedder, SimpleAST}
import squid.lang.ScalaCore
import squid.quasi.lift

@lift
class TestLift {
  def bar(x: Int):Int = x + 1
  def bar2(x: Int, s: String) : Unit = {

  }

  def test(): Unit = {
    println(bar(2))
  }

  /*def x[T](z: T) : Unit = {}*/

  var happines = 100
  def step(): Unit = {
    happines = happines - 10

    if (Math.random() < 0.5) {
      happines = happines + 20
    }
  }

}


object TestLift{

}

import squid.lang.Definitions
object TestingDSL extends SimpleAST with ClassEmbedder with ScalaCore with Definitions {

}

object Example2 extends App {

  val cls = TestLift.reflect(TestingDSL)

  for (m <- cls.methods) {
    println(m.symbol)
    println(m.typeParams)
    println(m.vparams)
    println(m.body)
  }
  println(cls.companion)
}
*/