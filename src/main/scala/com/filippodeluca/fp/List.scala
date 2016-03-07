package com.filippodeluca.fp

import scala.annotation.tailrec

sealed trait List[+A]

object List {

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  case object Nil extends List[Nothing]

  def empty[T]: List[T] = nil

  def nil[T]: List[T] = Nil

  def cons[T](h: T, tail: => List[T] = Nil): List[T] = {
    Cons(h, tail)
  }

  def head[T](xs: List[T]): Option[T] = xs match {
    case Nil => None
    case Cons(h, _) => Some(h)
  }

  def tail[T](xs: List[T]): List[T] =
    drop(xs, 1)

  /** Ex 3.3 */
  def setHead[T, T1 >: T](xs: List[T], x: T1): List[T1] =
    prepend(tail(xs), x)

  /** Ex 3.4 */
  def drop[A](xs: List[A], n: Int): List[A] = xs match {
    case Nil => Nil
    case l if n <= 0 => l
    case Cons(h, tail) if n > 0 => tail
  }


  def append[T, T1 >: T](xs: List[T], x: T1): List[T1] =
    reverse(prepend(reverse(xs), x))

  def prepend[T, T1 >: T](xs: List[T], x: T1): List[T1] =
    cons(x, xs)

  def size(xs: List[_]) = fold(xs)(0){ (acc, x) =>
    acc + 1
  }

  def reverse[A](xs: List[A]): List[A] = fold(xs)(empty[A]){(acc, x) =>
    prepend(acc, x)
  }

  def fold[A, B](xs: List[A])(zero: B)(f: (B, A) => B): B = {

    @tailrec
    def loop(list: List[A], acc: B): B = list match {
      case Cons(h, tail) => loop(tail, f(acc, h))
      case Nil => acc
    }

    loop(xs, zero)
  }


}


