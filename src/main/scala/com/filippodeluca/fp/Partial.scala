package com.filippodeluca.fp

trait Partial {

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    (b: B) => f(a, b)

}
