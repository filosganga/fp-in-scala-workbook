package com.filippodeluca.fp

trait Fibonacci {

  def fibonacci(n: Int): Int = {

    @scala.annotation.tailrec
    def loop(f1: Int, f2: Int, x: Int): Int = if (x == n) {
      f1 + f2
    } else {
      loop(f2, f1 + f2, x + 1)
    }


    n match {
      case 0 => 0
      case 1 => 1
      case _ => loop(0, 1, 2)
    }
  }

}
