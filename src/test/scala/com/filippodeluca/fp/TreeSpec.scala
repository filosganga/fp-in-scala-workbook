package com.filippodeluca.fp

import Tree._

class TreeSpec extends UnitSpec {

  "size" when {
    "the Tree is only a leaf" should {
      "return 1" in {
        Tree.size(leaf(1)) should be (1)
      }
    }

    "the Tree is a branch and two leafs" should {
      "return 3" in {
        Tree.size(branch(leaf(1), leaf(2))) should be (3)
      }
    }

    "the Tree is two branch and two leafs" should {
      "return 5" in {
        Tree.size(branch(leaf(1), branch(leaf(2), leaf(3)))) should be (5)
      }
    }
  }

  "maximum" when {
    "the Tree is only a leaf" should {
      "return the leaf value" in {
        maximum(leaf(1)) should be (1)
      }
    }

    "the Tree is bigger then one leaf" should {
      "return the maximum value" in {
        maximum(branch(leaf(1), branch(leaf(5), leaf(-9)))) should be (5)
      }
    }
  }

  "fold" should {
    "navigate in pre-order" in {
      List.reverse(fold(branch(leaf(1), branch(leaf(2), leaf(3))), List.nil[Int]) {
        case (acc, Some(x)) => List.prepend(acc, x)
        case (acc, _) => acc
      }) should be(List(1,2,3))
    }
  }

}
