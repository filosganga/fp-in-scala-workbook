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

    "be able to drop items" in forAll(){
      (xs: List[Int], n: Int) =>
        whenever(n >= 0) {
          val dropped = drop(xs, n)

          if(List.size(xs) <= n) {
            List.size(dropped) shouldBe 0
          } else {
            List.size(dropped) shouldBe List.size(xs) - n
          }
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
