package com.filippodeluca.fp

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._


class RngSpec extends UnitSpec {

  implicit val arbRng: Arbitrary[Rng] = Arbitrary {
    for {
      seed <- arbitrary[Long]
      rng <- new SimpleRng(seed)
    } yield rng
  }

  "Rng.randomPair" should {
    "return different values" in forAll() { rng: Rng =>
      Rng.randomPair.run(rng) match {
        case (one, two) => one should not be two
      }
    }
  }

  "Rng.double" should {
    "return number between 0 and 1" in forAll() { rng: Rng =>
      val double = Rng.double.run(rng)._1
      double should be <= 1.0
      double should be >= 0.0
    }

    "return the same value for the same rng" in forAll() { rng: Rng =>
      Rng.double.run(rng) shouldBe Rng.double.run(rng)
    }
  }

  "Rng.nonNegativeLessThan" should {
    "return number between 1 and n" in forAll() { (rng: Rng, n: Int) =>
      whenever(n > 1) {
        val (value, _) = Rng.nonNegativeLessThan(n).run(rng)

        value should (be >= 0 and be < n)
      }
    }
  }

  "Rng.rollDie" should {
    "return number between 1 and 6" in {
      val (value, _) = Rng.rollDie.run(SimpleRng(1L))
      value should (be >= 1 and be <= 600)
    }
  }

  "Rng.nonNegativeEven" should {
    "return numbers >= 0" in forAll() { rng: Rng =>

      val (value, _) = Rng.nonNegativeEven.run(rng)
      value should be >= 0
    }

    "return only even numbers" in forAll() { rng: Rng =>

      val (value, _) = Rng.nonNegativeEven.run(rng)
      value % 2 should be(0)
    }
  }

}
