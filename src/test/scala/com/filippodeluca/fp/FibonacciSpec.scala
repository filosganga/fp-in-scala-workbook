package com.filippodeluca.fp

import org.scalatest.{Matchers, WordSpec}

class FibonacciSpec extends UnitSpec with Fibonacci {

  "fibonacci" when {
    "the input is 0" should {
      "return 0" in {
        fibonacci(0) shouldBe 0
      }
    }

    "the input is 1" should {
      "return 1" in {
        fibonacci(1) shouldBe 1
      }
    }

    "the input is 2" should {
      "return 1" in {
        fibonacci(2) shouldBe 1
      }
    }

    "the input is 3" should {
      "return 2" in {
        fibonacci(2) shouldBe 1
      }
    }

    "the input is 20" should {
      "return 6765" in {
        fibonacci(20) shouldBe 6765
      }
    }

    "the input is very big" should {
      "be stack safe" in {
        fibonacci(10000)
      }
    }
  }

}
