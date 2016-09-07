package com.filippodeluca.fp

import Option._

class OptionSpec extends UnitSpec {

  "sequence" when {
    "the given list is empty" should {
      "return empty" in {
        Option.sequence(List.nil) should be (some(List.nil))
      }
    }

    "the given list does not contain any None" should {
      "return Some" in {
        Option.sequence(List(some(1), some(2), some(3))) should be(some(List(1,2,3)))

      }
    }

    "the given list does contain at least one None" should {
      "return None" in {
        Option.sequence(List(some(1), some(2), none)) should be(none)
      }
    }
  }

  "traverse" when {
    "the given list is empty" should {
      "return some of empty list" in {
        traverse(List.nil)(some) should be (some(List.nil))
      }
    }

    "the given list is not empty" when {
      "the given function return None for at least one item" should {
        "return None" in {

          traverse(List(1,2,3,4,5)){
            case 3 => none
            case x => some(x)
          } should be (none)
        }
      }

      "the given function return always some" should {
        "return some of the list of the results" in {

          traverse(List(1,2,3))(x=> some(x + 1)) should be (some(List(2,3,4)))
        }
      }
    }
  }

}
