package com.filippodeluca.fp

class CurrySpec extends UnitSpec with Curry {

  def add(x: Int, y: Int) = x + y
  def mul(x: Int, y: Int) = x * y

  "curry" should {
    "return a partially applied function" in {

      def add5 = curry(add)(5)

      add5(3) shouldBe 5 + 3
    }
  }

  "uncurry" should {
    "return a uncurried function" in {
      uncurry(curry(add))(5, 3) shouldBe 5 + 3
    }
  }

  "compose" should {
    "apply the given functions in correct order" in {
      val mul3AndThenAdd5 = compose(curry(add)(5), curry(mul)(3))

      mul3AndThenAdd5(3) shouldBe (3 * 3 + 5)
    }
  }
}
