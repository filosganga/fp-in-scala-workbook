package com.filippodeluca

/**
  *
  */
package object fp {

  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) {
    None
  } else {
    Some(xs.sum / xs.size)
  }

  def variance(xs: Seq[Double]): Option[Double] = for {
    m <- mean(xs)
    v <- mean(xs.map(x => math.pow(x - m, 2)))
  } yield v


  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  val liftedAbs: Option[Double] => Option[Double] = lift(math.abs)

}
