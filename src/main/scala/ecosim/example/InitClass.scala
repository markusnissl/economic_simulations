package ecosim.example

import ecosim.deep.member.Actor
import squid.quasi.lift

@lift
class InitClass {
  def main(): List[Actor] = {
    val m = new Market
    val f = new Farmer()
    f.market = m
    List(m, f)
  }
}
