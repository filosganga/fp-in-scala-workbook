package com.filippodeluca.fp

import scala.annotation.tailrec

sealed trait Stream[+A] {

  import Stream._

  def head: A =
    headOption.getOrElse(throw new NoSuchElementException("the stream is empty"))

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def tail: Stream[A] = this match {
    case Empty => Empty
    case Cons(_, t) => t()
  }

  def toList: List[A] = {

    @tailrec
    def loop(stream: Stream[A], list: List[A]): List[A] = stream match {
      case Empty => list
      case Cons(h, t) =>
        loop(t(), List.prepend(list, h()))
    }

    List.reverse(loop(this, List.nil))
  }

  def isEmpty = this == Empty

  def size: Int = foldLeft(0) { (s, _) => s + 1 }

  def foldLeft[T](zero: T)(f: (T, A) => T) = {

    @tailrec
    def loop(s: Stream[A], acc: T): T = s match {
      case Empty => acc
      case Cons(h, t) => loop(t(), f(acc, h()))
    }

    loop(this, zero)
  }

  def take(n: Int): Stream[A] = {

    def loop(stream: Stream[A], index: Int, target: Stream[A]): Stream[A] = stream match {
      case Empty => target
      case _ if index >= n => target
      case Cons(h, t) =>
        loop(t(), index + 1, cons(h(), target))
    }

    loop(this, 0, Stream.empty).reverse
  }

  def reverse: Stream[A] = foldLeft(Stream.empty[A]) { (accu, item) => Stream.cons(item, accu) }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Empty => this
    case _ if n <= 0 => this
    case Cons(h, t) => t().drop(n - 1)
  }

  def foldRight[T](z: => T)(f: (A, => T) => T): T = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true) { (a, acc) =>
    p(a) && acc
  }

  def exists(p: A => Boolean): Boolean = foldRight(false) { (a, t) =>
    p(a) || t
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A]) { (a, acc) =>
      if (!p(a)) {
        Stream.empty
      } else {
        cons(a, acc)
      }
    }


  def headOptionWithFoldRight: Option[A] = foldRight(Option.none[A]) { (a, acc) =>
    Some(a)
  }

  def filter(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A]) { (a, acc) =>
    if (p(a))
      cons(a, acc)
    else
      acc
  }

  def append[A1 >: A](a: => A1): Stream[A1] = foldRight(cons(a, empty)) { (a, acc) =>
    cons(a, acc)
  }

  def append[A1 >: A](that: Stream[A1]): Stream[A1] = foldRight(that) { (a, acc) =>
    cons(a, acc)
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B]) { (a, bs) =>
    f(a).append(bs)
  }

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Empty => None
    case Cons(h, t) => Some(f(h()) -> t())
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold(this) {
    case Empty => None
    case _ if n <= 0 => None
    case Cons(h, t) => Some(h() -> t().takeViaUnfold(n - 1))
  }

  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] = unfold(this -> bs) {
    case (Empty, Empty) => None
    case (l, r) =>
      Some((l.headOption -> r.headOption, l.tail -> r.tail))
  }

  def zipWith[B, C](bs: Stream[B])(combine: (A, B) => C): Stream[C] = unfold(this -> bs) {
    case (Cons(ah, at), Cons(bh, bt)) => Some(combine(ah(), bh()) ->(at(), bt()))
    case _ => None
  }


  def zip[B](bs: Stream[B]): Stream[(A, B)] =
    zipWith(bs)(_ -> _)

  def startsWith[A1 >: A](s: Stream[A1]): Boolean =
    zipWith(s)(_ == _).forAll(identity)

}

object Stream {

  case object Empty extends Stream[Nothing]

  case class Cons[A](h: () => A, t: () => Stream[A]) extends Stream[A]

  def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
    lazy val lazyHead = head
    lazy val lazyTail = tail
    Cons(() => lazyHead, () => lazyTail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](xs: A*): Stream[A] =
    if (xs.isEmpty) empty else cons(xs.head, apply(xs.tail: _*))

  def constant[A](a: A): Stream[A] = unfold(a)(x => Some(x, x))

  def from(n: Int): Stream[Int] = unfold(n) { s =>
    Some(s, s + 1)
  }

  def fibs: Stream[Int] = Stream(0, 1) append unfold((0, 1)) { case (fibA, fibB) =>
    val nextFib = fibA + fibB
    Some(nextFib, fibB -> nextFib)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((nextValue, nextStatus)) => cons(nextValue, unfold(nextStatus)(f))
    case None => empty
  }

  def continuosly[A](generator: () => A): Stream[A] =
    unfold(generator()) { s =>
      Some(s, generator())
    }


}