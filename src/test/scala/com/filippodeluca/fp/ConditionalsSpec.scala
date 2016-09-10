package com.filippodeluca.fp

import org.scalatest.FunSuite


class ConditionalsSpec extends UnitSpec {

  import Conditionals._

  "and" when {
    "left side is true" when {
      "right side is true" should {
        "return true" in {
          true and true shouldBe true
        }
      }
      "right side is false" should {
        "return false" in {
          true and false shouldBe false
        }
      }
    }

    "left side is false" when {
      "right side is true" should {
        "return false" in {
          false and true shouldBe false
        }
      }
      "right side is false" should {
        "return false" in {
          false and false shouldBe false
        }
      }
    }
  }

  "or" when {
    "left side is true" when {
      "right side is true" should {
        "return true" in {
          true or true shouldBe true
        }
      }
      "right side is false" should {
        "return true" in {
          true or false shouldBe true
        }
      }
    }

    "left side is false" when {
      "right side is true" should {
        "return true" in {
          false or true shouldBe true
        }
      }
      "right side is false" should {
        "return false" in {
          false or false shouldBe false
        }
      }
    }
  }

  "not" when {
    "argument is false" should {
      "return true" in {
        Conditionals.not(false) shouldBe true
      }
    }

    "argument is true" should {
      "return false" in {
        Conditionals.not(true) shouldBe false
      }
    }
  }

}
