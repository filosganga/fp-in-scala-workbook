package com.filippodeluca.fp

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def leaf[A](a: A): Tree[A] = Leaf(a)

  def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)

  def size(t: Tree[_]): Int =
    fold(t, 0){ (acc, _) => acc + 1}

  def maximum[A](t: Tree[A])(implicit n: Ordering[A]): A =
    fold(t, None: Option[A]){
      case (None, x) => x
      case (x, None) => x
      case (Some(x), Some(y)) =>
        Some(n.max(x, y))
    }.getOrElse(throw new NoSuchElementException)


  def fold[A, B](t: Tree[A], zero: B)(f: (B, Option[A]) => B): B = {

    def loop(currentBranch: Tree[A], branches: List[Tree[A]], accumulator: B): B = (currentBranch, branches) match {
      case (Leaf(a), List.Cons(h, tail)) =>
        loop(h, tail, f(accumulator, Some(a)))
      case (Leaf(a), List.Nil) =>
        f(accumulator, Some(a))
      case (Branch(l, r), _) =>
        loop(l, List.prepend(branches, r), f(accumulator, None))
    }

    loop(t, List.nil, zero)
  }
}