package com.filippodeluca.fp

object Either {

  def left[E](e: E): Either[E, Nothing] = Left(e)

  def right[A](a: A): Either[Nothing, A] = Right(a)

  def Try[A](a: => A): Either[Exception, A] =
    try
      Right(a)
    catch {
      case e: Exception => Left(e)
    }

  def map2[E, A, B, C](e1: Either[E, A], e2: Either[E, B])(f: (A, B) => C): Either[E, C] = for {
    a <- e1
    b <- e2
  } yield f(a, b)

  def map3[E, A, B, C, D](e1: Either[E, A], e2: Either[E, B], e3: Either[E, C])(f: (A, B, C) => D): Either[E, D] = for {
    a <- e1
    b <- e2
    c <- e3
  } yield f(a, b, c)

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(identity)

  def traverse[E, A, B](xs: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    List.foldLeft[A, Either[E, List[B]]](xs, right[List[B]](List.nil[B])){(acc, x) =>
      map2(acc, f(x))(List.prepend(_, _))
    }.map(List.reverse)
}

sealed trait Either[+E, +A] {
  import Either._

  def flatMap[E1 >: E, B](f: A => Either[E1, B]): Either[E1, B] = this match {
    case Right(a) => f(a)
    case l: Left[E] => l
  }

  def map[B](f: A => B): Either[E, B] =
    flatMap(a => right(f(a)))

  def map2[E1 >: E, B, C](b: Either[E1, B])(f: (A, B) => C): Either[E1, C] =
    Either.map2(this, b)(f)

  def orElse[E1 >: E, A1 >: A](default: => Either[E1, A1]): Either[E1, A1] = this match {
    case r: Right[A] => r
    case l: Left[_] => default
  }

  def getOrElse[A1 >: A](default: => A1): Either[E, A1] =
    orElse(right(default))

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

