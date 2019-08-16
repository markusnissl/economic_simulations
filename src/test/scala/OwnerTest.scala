import org.scalatest._
import old.Commodities._
import old.Owner.Owner


class OwnerSpec extends FlatSpec {

  class O2 extends Owner {
    def mycopy = { val o = new O2; copy_state_to(o); o }
  }

  "Owner" should "deep-copy inventory" in {
    val a = new O2;
    //a.init_inv(Flour);
    a.make(Flour, 1, 1000);

    val b = a.mycopy;

    assert(b.available(Flour) == 1)

    a.make(Flour, 1, 1000);

    assert(a.available(Flour) == 2)
    assert(b.available(Flour) == 1)

    b.make(Flour, 1, 1000);
    b.make(Flour, 1, 1000);

    assert(a.available(Flour) == 2)
    assert(b.available(Flour) == 3)
  }
}

