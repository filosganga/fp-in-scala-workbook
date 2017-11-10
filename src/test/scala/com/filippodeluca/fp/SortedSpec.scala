package com.filippodeluca.fp

class SortedSpec extends UnitSpec with Sorted {

  "isSorted" when {
    "the input array is empty" should {
      "return true" in {
        isSorted[Int](Array.empty, (l,r) => l <= r) shouldBe true
      }
    }

    "the input array contains only one element" should {
      "return true" in {
        isSorted[Int](Array(1), (l,r) => l <= r) shouldBe true
      }
    }

    "the input array contains more than one element not sorted" should {
      "return false" in {
        isSorted[Int](Array(1, 4, 3), (l,r) => l <= r) shouldBe false
      }
    }

    "the input array contains more than one element sorted according to the given function" should {
      "return true" in {
        isSorted[Int](Array(1, 3, 4, 4, 5, 78), (l,r) => l <= r) shouldBe true
      }
    }
  }

}
