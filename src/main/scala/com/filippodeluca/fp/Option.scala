package com.filippodeluca.fp


trait Option[+A] {

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def map[B](f: A => B): Option[B] =
    flatMap(f.andThen(Some.apply))

  def orElse[B >: A](default: => Option[B]): Option[B] =
    map(Some.apply).getOrElse(default)

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

}
case class Some[+A](a: A) extends Option[A]
case object None extends Option[Nothing]


object Option {

  def some[A](a: A): Option[A] = Some(a)

  def none[A]: Option[A] = None

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
    xa <- a
    xb <- b
  } yield f(xa, xb)

  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(identity)

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    List.foldLeft(as, some(List.nil[B])){ (acc, x) =>
      map2(acc, f(x))(List.prepend(_, _))
    }.map(List.reverse)
  }

}