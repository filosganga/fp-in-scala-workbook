package com.filippodeluca.fp

sealed trait Validated[+E, +A] {

  import Validated._

  def flatMap[E1 >: E, B](f: A => Validated[E1, B]): Validated[E1, B] = this match {
    case Valid(a) => f(a)
    case l: Invalid[E] => l
  }

  def map[B](f: A => B): Validated[E, B] =
    flatMap(a => valid(f(a)))

  def map2[E1 >: E, B, C](b: Validated[E1, B])(f: (A, B) => C): Validated[E1, C] =
    Validated.map2(this, b)(f)

}
case class Invalid[+E](errors: List[E]) extends Validated[E, Nothing]
case class Valid[+A](value: A) extends Validated[Nothing, A]


object Validated {

  def valid[E, A](value: A): Validated[E, A] = Valid(value)

  def invalid[E, A](errors: List[E]) = Invalid(errors)

  def map2[E, A, B, C](e1: Validated[E, A], e2: Validated[E, B])(f: (A, B) => C): Validated[E, C] = (e1, e2) match {
    case (Invalid(e1Errors), Invalid(e2Errors)) => Invalid(List.prepend(e2Errors, e1Errors))
    case (Valid(_), r: Invalid[E]) => r
    case (l: Invalid[E], Valid(_)) => l
    case (Valid(e1Value), Valid(e2Value)) => Valid(f(e1Value, e2Value))
  }

  def traverse[E, A, B](xs: List[A])(f: A => Validated[E, B]): Validated[E, List[B]] =
    List.foldLeft(xs, valid[E, List[B]](List.nil[B])){(acc, x) =>
      map2(acc, f(x))(List.prepend(_, _))
    }.map(List.reverse)
}
