package com.filippodeluca.fp

import com.filippodeluca.fp.List.{Cons, Nil}

sealed trait List[+A]

trait ListOps {

  def cons[A](h: A, tail: List[A]): List[A] = Cons(h, tail)

  def nil[A]: List[A] = Nil


  def tail[A](xs: List[A]): List[A] = xs match {
    case Cons(_, ts) => ts
    case Nil => Nil
  }

}

object List extends ListOps {

  case object Nil extends List[Nothing]

  case class Cons[+A](h: A, tail: List[A]) extends List[A]

  def apply[A](as: A*): List[A] = if (as.isEmpty)
    Nil
  else
    Cons(as.head, apply(as.tail: _*))

}