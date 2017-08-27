package com.filippodeluca.fp


trait Rng {
  def nextInt: (Int, Rng)
}

object Rng {

  type Rand[+A] = State[Rng, A]

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = for {
    a <- ra
    b <- rb
  } yield a -> b

  val int: Rand[Int] =
    State(_.nextInt)

  val randomPair: Rand[(Int, Int)] =
    both(int, int)

  val nonNegativeInt: Rand[Int] = for {
    x <- int
  } yield math.abs(x)

  val double: Rand[Double] =
    nonNegativeInt.map(_.toDouble / Int.MaxValue)

  val nonNegativeEven: Rand[Int] =
    nonNegativeInt.map(i => i - i % 2)

  def ints(count: Int): Rand[List[Int]] =
    State.sequence(List.fill(count)(int))

  def intDouble(rng: Rng): Rand[(Int, Double)] =
    both(int, double)

  def doubleInt(rng: Rng): Rand[(Double, Int)] =
    both(double, int)

  def double3(rng: Rng): Rand[(Double, Double, Double)] = for {
    a <- double
    b <- double
    c <- double
  } yield (a, b, c)

  def nonNegativeLessThan(n: Int): Rand[Int] = for {
    i <- nonNegativeInt
    if i < n
  } yield i

  val rollDie: Rand[Int] =
    nonNegativeLessThan(6).map(_ + 1)

}

case class SimpleRng(seed: Long) extends Rng {
  override def nextInt: (Int, Rng) = {
    val nextSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val n = (nextSeed >>> 16).toInt
    n -> SimpleRng(nextSeed)
  }
}