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
