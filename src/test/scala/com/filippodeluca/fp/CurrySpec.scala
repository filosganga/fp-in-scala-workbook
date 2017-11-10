package com.filippodeluca.fp

class CurrySpec extends UnitSpec with Curry {

  "curry" should {
    "return a partially applied function" in {
      def add(x: Int, y: Int) = x + y

      def add5 = curry(add)(5)

      add5(3) shouldBe 5 + 3
    }
  }

  "uncurry" should {
    "return a uncurried function" in {
      def add(x: Int, y: Int) = x + y


      uncurry(curry(add))(5, 3) shouldBe 5 + 3
    }
  }
}
