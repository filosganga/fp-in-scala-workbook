package com.filippodeluca.fp

import scala.annotation.tailrec

sealed trait List[+A]

object List {

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  case object Nil extends List[Nothing]

  def nil[T]: List[T] = Nil

  def cons[T](h: T, tail: => List[T] = Nil): List[T] = {
    Cons(h, tail)
  }

  def apply[T](xs: T*): List[T] = {
    reverse(xs.foldLeft(nil[T]){(acc, x) =>
      cons(x, acc)
    })
  }

  def fill[A](n: Int)(a: A): List[A] = Stream.unfold(0){
    case i if i < n => Some(a->(i+1))
    case _ => None
  }.toList

  def head[T](xs: List[T]): Option[T] = xs match {
    case Nil => None
    case Cons(h, _) => Some(h)
  }

  /**
    * Ex 3.6
    *
    * Return all element od the list but not the last
    */
  def init[A](xs: List[A]): List[A] =
    reverse(tail(foldLeft(xs, nil[A]) { (acc, x) =>
      prepend(acc, x)
    }))

  def tail[T](xs: List[T]): List[T] =
    drop(xs, 1)

  /** Ex 3.3 */
  def setHead[T, T1 >: T](xs: List[T], x: T1): List[T1] =
    prepend(tail(xs), x)

  /** Ex 3.4 */
  @tailrec
  def drop[A](xs: List[A], n: Int): List[A] = xs match {
    case Nil => Nil
    case l if n <= 0 => l
    case Cons(h, tail) if n > 0 => drop(tail, n - 1)
  }

  /** Ex 3.5 */
  @tailrec
  def dropWhile[A](xs: List[A])(f: A => Boolean): List[A] = head(xs) match {
    case Some(x) if f(x) => dropWhile(tail(xs))(f)
    case _ => xs
  }

  def append[T, T1 >: T](xs: List[T], x: T1): List[T1] =
    reverse(prepend(reverse(xs), x))

  def append[A](xs: List[A], ys: List[A]): List[A] = xs match {
    case Nil => ys
    case Cons(h, t) => Cons(h, append(t, ys))
  }

  def prepend[T, T1 >: T](xs: List[T], x: T1): List[T1] =
    cons(x, xs)

  def prepend[T, T1 >: T](xs: List[T], ys: List[T1]): List[T1] = ys match {
    case Nil => xs
    case Cons(h, t) => prepend(prepend(xs, h), t)
  }

  def size(xs: List[_]): Int = foldLeft(xs, 0) { (acc, x) =>
    acc + 1
  }

  def reverse[A](xs: List[A]): List[A] = foldLeft(xs, nil[A]) { (acc, x) =>
    prepend(acc, x)
  }

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {

    @tailrec
    def loop(l: List[A], acc: B): B = l match {
      case Nil => acc
      case Cons(h, tail) => loop(tail, f(acc, h))
    }

    loop(as, z)
  }

  def foldRight[A, B](xs: List[A], zero: B)(f: (A, B) => B): B =
    foldLeft(reverse(xs), zero) { (b,a) => f(a,b)}

  def flatten[A](xss: List[List[A]]): List[A] =
    reverse(foldLeft(xss, List.nil[A]){ (acc, xs) =>
      foldLeft(xs, acc)(prepend(_, _))
    })

  def increment[A](xs: List[A])(implicit n: Numeric[A]): List[A] =
    map(xs){n.plus(_, n.one)}

  def map[A, B](xs: List[A])(f: A => B): List[B] =
    reverse(foldLeft(xs, List.nil[B]){ (acc, x) =>
      prepend(acc, f(x))
    })

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as){
      case a if f(a) => List(a)
      case _ => nil
    }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    flatten(map(as)(f))

  def zip[A, B](as: List[A], bs: List[B]): List[(A, B)] =
    zipWith(as, bs)(_ -> _)

  def zipWith[A, B, C](as: List[A], bs: List[B])(combine: (A, B) => C): List[C] = {

    @tailrec
    def loop(al: List[A], bl: List[B], acc: List[C]): List[C] = al->bl match {
      case (Cons(aHead, aTail), Cons(bHead, bTail)) => loop(aTail, bTail, prepend(acc, combine(aHead, bHead)))
      case _ => acc
    }

    reverse(loop(as, bs, List.nil[C]))
  }

  def hasSubsequence[A](xs: List[A], zs: List[A]): Boolean = {

    def loop(xs: List[A]): Boolean = xs match {
      case Cons(_, _) if zipWith(xs, zs)(_ == _) == map(zs)(_ => true) => true
      case Cons(_, tail) => loop(tail)
      case Nil => false
    }

    loop(xs)
  }

  def splitWhere[A](xs: List[A])(predicate: A => Boolean): List[List[A]] = {
    foldRight(xs, List.cons(List.nil[A], List.nil)){
      case (y, l@Cons(Nil, _)) if predicate(y) => l
      case (y, l) if predicate(y) => List.cons(List.nil, l)
      case (y, Cons(h, tail)) if !predicate(y) => prepend(tail, prepend(h, y))
    }
  }

}


