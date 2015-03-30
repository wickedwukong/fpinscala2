package fpinscala.monoids

import org.scalatest.{Matchers, FlatSpec}
import fpinscala.monoids.Monoid._
import Matchers._

class MonoidSpec extends FlatSpec{ 

  "intAddition" should "follow monoid associative laws" in {
       assert(intAddition.op(intAddition.op(1, 2), 3) == intAddition.op(1, intAddition.op(2, 3)))
    }

  "intAddition" should "have a zero following monoid identity laws" in {
       assert(intAddition.op(intAddition.zero, 3) == 3)
       assert(intAddition.op(3, intAddition.zero) == 3)
    }

  "booleanOr" should "follow monoid associative laws" in {
       assert(booleanOr.op(true, false) == booleanOr.op(false, true))
       assert(booleanOr.op(booleanOr.op(true, true), true) == booleanOr.op(true, booleanOr.op(true, true)))
       assert(booleanOr.op(booleanOr.op(false, true), true) == booleanOr.op(false, booleanOr.op(true, true)))
    }

  "booleanOr" should "have a zero following monoid identity laws" in {
       assert(booleanOr.op(booleanOr.zero, true))
       assert(booleanOr.op(true, booleanOr.zero))
       booleanOr.op(booleanOr.zero, false) should be(false)
       booleanOr.op(false, booleanOr.zero) should be(false)
    }
}