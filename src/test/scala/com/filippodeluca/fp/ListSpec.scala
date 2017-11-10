package com.filippodeluca.fp

class ListSpec extends UnitSpec with ListOps {

  "List.tail" when {
    "the list is nil" should {
      "return nil" in {
        tail(nil[Int]) shouldBe nil[Int]
      }
    }

    "the list has length 1" should {
      "return nil" in {
        tail(List(1)) shouldBe nil[Int]
      }
    }

    "the list has several elements" should {
      "return the list without the head" in {
        tail(List(1, 2, 3, 4, 5)) shouldBe List(2, 3, 4, 5)
      }
    }
  }

}
