package com.filippodeluca.fp

import org.scalacheck.{Arbitrary, Gen}

class ListSpec extends UnitSpec {

  import List._

  "a List" should {

    "be able to prepend item" in forAll(){
      xs: List[Int] =>

      prepend(xs, 0) should be(cons(0, xs))
    }

    "be able to append item" in forAll() {
      xs: List[Int] =>

      append(xs, 3) should be(reverse(prepend(reverse(xs), 3)))
    }

    "be able to replace head item" in forAll(){
      (xs: List[Int], x: Int) =>
        whenever(List.size(xs) > 0) {
          head(setHead(xs, x)) shouldBe Some(x)
        }
    }
  }

  "drop" when {
    "the list is empty" should {
      "return Nil" in forAll(){
        n: Int =>
        drop(List.empty, n) should be (List.empty)
      }
    }

    "the list size is <= n" should {
      "return Nil" in forAll(){
        (xs: List[Int], n: Int) =>
          whenever(List.size(xs) <= n) {
            drop(xs, n) should be(List.empty)
          }
      }
    }
  }

  "dropWhile" when {
    "the list is Nil" should {
      "return Nil" in {
        dropWhile(List.empty[Int])(_ => true) shouldBe List.empty[Int]
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
        init(List(1,2,3,4)) shouldBe List(1,2,3)
      }
    }
  }

  "foldRight" when {
    "the list is empty" should {
      "return the zero" in forAll(){
        zero: Int =>
          foldRight(nil[Int], zero){ (x, sum) =>
            x + sum
          } should be(zero)
      }
    }

    "the list is not empty" should {
      "aggregate from right to left" in {
        foldRight(List(1,2,3), 0){ (x, acc) =>
          x
        } should be(1)
      }

      "duplicate the list when nil and cons are passed" in forAll(){
        l: List[Int] =>

        foldRight(l, nil[Int])(cons(_, _)) should be (l)

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
