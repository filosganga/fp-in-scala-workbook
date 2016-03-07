package com.filippodeluca.fp

import scala.annotation.tailrec

sealed trait Stream[+A] {
  import Stream._

  def head: A = headOption.get

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => List.empty
    case Cons(h, t) => List.cons(h(), t().toList)
  }

  def isEmpty = this == Empty

  def size: Int = foldLeft(0){(s,_) => s + 1}

  def foldLeft[T](zero: T)(f: (T, A) => T) = {

    @tailrec
    def loop(s: Stream[A], acc: T): T = s match {
      case Empty => acc
      case Cons(h, t) => loop(t(), f(acc, h()))
    }

    loop(this, zero)
  }

  def take(n: Int): Stream[A] = ???

  def drop(n: Int): Stream[A] = ???
}

object Stream {

  case object Empty extends Stream[Nothing]

  case class Cons[A](h: () => A, tail: () => Stream[A]) extends Stream[A]

  def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
    lazy val lazyHead = head
    lazy val lazyTail = tail
    Cons(() => lazyHead, () => lazyTail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](xs: A*): Stream[A] =
    if (xs.isEmpty) empty else cons(xs.head, apply(xs.tail:_*))
}