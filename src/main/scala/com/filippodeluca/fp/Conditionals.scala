package com.filippodeluca.fp


object Conditionals {

  implicit class RichBoolean(val l: Boolean) extends AnyVal {

    def and(r: => Boolean): Boolean =
      if (l) r else false

    def or(r: => Boolean): Boolean =
      if(l) true else r

  }

  def not(rb: RichBoolean): Boolean = !rb.l

  not(true and false) and not(false or true)

}
