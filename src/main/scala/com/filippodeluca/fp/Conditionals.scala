package com.filippodeluca.fp


object Conditionals extends App {

  implicit class RichBoolean(val l: Boolean) extends AnyVal {

    def and(r: => Boolean): Boolean =
      if (l) r else false

    def or(r: => Boolean): Boolean =
      if(l) true else r

    def unary_not: Boolean = !l
  }

}
