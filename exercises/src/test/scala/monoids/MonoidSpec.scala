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

  "optionMonoid" should "follow monoid associative laws" in {
    assert(optionMonoid.op(Some(1), Some(2)) == Some(1))
    assert(optionMonoid.op(None, None) == None)
    assert(optionMonoid.op(Some(1), None) == Some(1))

    assert(optionMonoid.op(Some(1), None) == optionMonoid.op(None, Some(1)))
    assert(optionMonoid.op(optionMonoid.op(Some(1), Some(2)), Some(3)) == optionMonoid.op(Some(1), optionMonoid.op(Some(2), Some(3))))
    assert(optionMonoid.op(optionMonoid.op(None, Some(1)), Some(2)) == optionMonoid.op(None, optionMonoid.op(Some(1), Some(2))))
  }

  "optionMonoid" should "have a zero following monoid identity laws" in {
    optionMonoid.op(optionMonoid.zero, Some(1)) should be(Some(1))
    optionMonoid.op(Some(1), optionMonoid.zero) should be(Some(1))
    optionMonoid.op(optionMonoid.zero, None) should be(None)
    optionMonoid.op(None, optionMonoid.zero) should be(None)
  }

  "endoMonoid" should "follow monoid associative laws" in {
    val f1: Int => Int = i => 1
    val f2: Int => Int = i => 2
    val f3: Int => Int = i => 3

    endoMonoid.op(f1, f2)(100) should be(2)
    endoMonoid.op(f1, f2)(100) should be(f2(2))
//    endoMonoid.op(f2, f1) should be(f1(1))

    assert(endoMonoid.op(endoMonoid.op(f1, f2), f3)(1000) == endoMonoid.op(f1, endoMonoid.op(f2, f3))(1000))
  }

//  "intAddition" should "have a zero following monoid identity laws" in {
//    assert(intAddition.op(intAddition.zero, 3) == 3)
//    assert(intAddition.op(3, intAddition.zero) == 3)
//  }


}