package com.filippodeluca.fp

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.enablers.Sequencing

class ListSpec extends UnitSpec {

  import List._

  "a List" should {

    "be able to prepend item" in forAll() { xs: List[Int] =>
      prepend(xs, 0) should be(cons(0, xs))
    }

    "be able to append item" in forAll() { xs: List[Int] =>
      append(xs, 3) should be(reverse(prepend(reverse(xs), 3)))
    }

    "be able to replace head item" in forAll() { (xs: List[Int], x: Int) =>
      whenever(List.size(xs) > 0) {
        head(setHead(xs, x)) shouldBe Some(x)
      }
    }
  }

  "drop" when {
    "the list is empty" should {
      "return Nil" in forAll() {
        n: Int =>
          drop(List.nil, n) should be(List.nil)
      }
    }

    "the list size is <= n" should {
      "return Nil" in forAll() {
        (xs: List[Int], n: Int) =>
          whenever(List.size(xs) <= n) {
            drop(xs, n) should be(List.nil)
          }
      }
    }
  }

  "dropWhile" when {
    "the list is Nil" should {
      "return Nil" in {
        dropWhile(List.nil[Int])(_ => true) shouldBe List.nil[Int]
      }
    }

    "the list is not empty" should {
      "return the element of the list that match the predicate" in {
        val xs: List[Int] = cons(1, cons(2, cons(3, cons(4, cons(1, nil)))))
        dropWhile(xs)(_ <= 3) shouldBe cons(4, cons(1, nil))
      }

      "return the list when the first element does not match the predicate" in {
        val xs: List[Int] = cons(5, cons(2, cons(3, cons(4, cons(1, nil)))))
        dropWhile(xs)(_ <= 3) shouldBe xs
      }
    }
  }

  "init" when {
    "the list is empty" should {
      "return empty" in {
        init(nil) shouldBe nil
      }
    }
    "the list has one element" should {
      "return empty" in {
        init(cons(1, nil)) shouldBe nil
      }
    }
    "the list has more then one element" should {
      "return all element but last" in {
        init(List(1, 2, 3, 4)) shouldBe List(1, 2, 3)
      }
    }
  }

  "foldRight" when {
    "the list is empty" should {
      "return the zero" in forAll() {
        zero: Int =>
          foldRight(nil[Int], zero) { (x, sum) =>
            x + sum
          } should be(zero)
      }
    }

    "the list is not empty" should {
      "aggregate from right to left" in {
        foldRight(List(1, 2, 3), 0) { (x, acc) =>
          x
        } should be(1)
      }

      "duplicate the list when nil and cons are passed" in forAll() {
        l: List[Int] =>

          foldRight(l, nil[Int])(cons(_, _)) should be(l)

      }
    }
  }

  "flatten" when {
    "the list is empty" should {
      "return empty" in {
        flatten(nil) should be(nil)
      }
    }

    "the list contains only one list" should {
      "return the head" in forAll() {
        xs: List[Int] =>
          flatten(List(xs)) should be(xs)
      }
    }

    "the list contains only multiple lists" should {
      "return the lists element in order" in {
        flatten(List(List(1, 2, 3), List(4, 5))) should be(List(1, 2, 3, 4, 5))
      }
    }
  }

  "increment" when {
    "the list is empty" should {
      "return empty" in {
        increment(nil[Int]) should be(nil[Int])
      }
    }

    "the list is not empty" should {
      "return a list with elements increased by one" in {
        increment(List(1, 2, 3)) should be(List(2, 3, 4))
      }
    }
  }

  "filters" should {
    "remove the elemnets that does not satisfy the predicate" in {
      filter(List(1, 2, 3, 4, 5))(_ % 2 == 0) should be(List(2, 4))
    }
  }

  "zipWith" when {
    "one of the first list is empty" should {
      "return empty" in {
        zip(nil[Int], List(1, 2, 3)) should be(nil)
        zip(List(1, 2, 3), nil[Int]) should be(nil)
        zip(nil[Int], nil[Int]) should be(nil)
      }
    }

    "one of the list is shorter" should {
      "return a list whit size equal to the shorter one" in forAll() {
        (as: List[Int], bs: List[Int]) =>
          List.size(zip(as, bs)) should be(math.min(List.size(as), List.size(bs)))
      }
    }

    "no list is empty" should {
      "return a list with element combined" in {
        zipWith(List('a', 'b'), List(1, 2)) { (a, b) => s"$a$b" } should be(List("a1", "b2"))
      }
    }

  }

  "hasSubsequence" when {
    "the list is empty" should {
      "return false" in {
        hasSubsequence(nil, List(1, 2, 3)) shouldBe false
      }
    }

    "the sub-sequence is empty" should {
      "return true" in {
        hasSubsequence(List(1, 2, 3), nil) shouldBe true
      }
    }

    "the list contains the sub-sequence" should {
      "return true" in {
        hasSubsequence(List(1, 2, 3, 4), List(2, 3)) shouldBe true
      }
    }

    "the list does not contains the sub-sequence" should {
      "return false" in {
        hasSubsequence(List(1, 2, 3, 4), List(2, 4)) shouldBe false
      }
    }
  }

  "splitWhere" when {
    "the list is empty" should {
      "return empty list in the head" in {
        List.head(splitWhere(List.nil[Int])(_ == 3)) shouldBe Some(List.nil)
      }
    }

    "the predicate always matches" should {
      "return empty list in the head" in forAll() { xs: List[Int] =>
        List.head(splitWhere(xs)(_ => true)) shouldBe Some(List.nil)
      }
    }

    "the predicate never matches" should {
      "return the original list in the first element" in forAll() { xs: List[Int] =>
        List.head(splitWhere(xs)(_ => false)) shouldBe Some(xs)
      }
    }

    "the predicate matches few elements" should {
      "return the list of sublists" in {
        splitWhere(List(1, 2, 0, 1, 2, 3, 0, 0, 1, 2, 3, 4, 0))(_ == 0) shouldBe List(List(1, 2), List(1, 2, 3), List(1, 2, 3, 4))
      }
    }

  }

  implicit def listArb[T](implicit arbT: Arbitrary[T]): Arbitrary[List[T]] =
    Arbitrary(listGen[T](arbT.arbitrary))

  def listGen[T](implicit tGen: Gen[T]): Gen[List[T]] =
    Gen.oneOf(nilGen[T], consGen)

  def nilGen[T] =
    Gen.const(nil[T])

  def consGen[T](implicit tGen: Gen[T]): Gen[List[T]] = for {
    head <- tGen
    tail <- listGen[T]
  } yield cons(head, tail)
}
