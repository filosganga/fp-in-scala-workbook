package com.filippodeluca.fp

import scala.annotation.tailrec

case class State[S, +A](run: S => (A, S)) {
  import State._

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s: S =>
    val (a, newS) = run(s)
    f(a).run(newS)
  }

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  val filter = withFilter _

  def withFilter(p: A => Boolean): State[S, A] = State { s =>

    @tailrec
    def loop(prevS: S): (A, S) = {
      val (a,s) = run(prevS)

      if(p(a)) {
        a->s
      } else {
        loop(s)
      }
    }

    loop(s)
  }
}

object State {

  def unit[S, A](a: A): State[S, A] = State { s =>
    (a, s)
  }

  def sequence[S, A](xs: List[State[S, A]]): State[S, List[A]] = {
    List.foldRight(xs, State.unit[S, List[A]](List.nil[A])) { (aState, listState) =>
      for {
        a <- aState
        list <- listState
      } yield List.prepend(list, a)
    }
  }
}