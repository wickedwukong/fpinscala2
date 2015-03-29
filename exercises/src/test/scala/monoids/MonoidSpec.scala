package fpinscala.monoids

import org.scalatest.FlatSpec
import fpinscala.monoids.Monoid._

class MonoidSpec extends FlatSpec{ 

  "intAddition" should "follow monoid associative laws" in {
       assert(intAddition.op(intAddition.op(1, 2), 3) == intAddition.op(1, intAddition.op(2, 3)))
    }
}