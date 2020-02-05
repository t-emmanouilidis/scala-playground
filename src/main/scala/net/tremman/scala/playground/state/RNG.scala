package net.tremman.scala.playground.state

import scala.annotation.tailrec

// random number generator
trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  // simple RNG implementation that needs a long seed
  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  // a state transition with a RNG that returns non negative numbers
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRng) = rng.nextInt

    (if (n < 0) -(n + 1) else n, nextRng)
  }

  // a state transition with a RNG that returns double numbers
  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRng) = nonNegativeInt(rng)
    val d: Double = n.toDouble / Int.MaxValue
    (d, nextRng)
  }

  // a state transition with a RNG that returns a tuple of integer and double numbers
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, nextRng) = rng.nextInt
    val (d, nextRng2) = double(nextRng)
    ((n, d), nextRng2)
  }

  // a state transition with a RNG that returns a tuple of double and integer number
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((n, d), nextRng) = intDouble(rng)
    ((d, n), nextRng)
  }

  // a RNG that returns a tuple of three double numbers
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  // a RNG that returns a list of n integer numbers
  def ints(n: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(currentCount: Int, currentRng: RNG, theInts: List[Int]): (List[Int], RNG) = {
      if (currentCount > 0) {
        val (next, rng2) = currentRng.nextInt
        go(currentCount - 1, rng2, next :: theInts)
      } else
        (Nil, rng)
    }

    go(n, rng, List())
  }

  type Rand[A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  // Some additional generate state transitions
  def nonNegative: Rand[Int] = map(nonNegativeInt)(n => n)

  def nonNegativeEven: Rand[Int] = map(nonNegative)(i => i - i % 2)

  def boolean: Rand[Boolean] = map(nonNegative)(i => i % 2 == 0)

  def nonNegativeDouble: Rand[Double] = map(nonNegative)(n => n.toDouble)

  val intDouble2: Rand[(Int, Double)] = product(nonNegative, nonNegativeDouble)

  val doubleInt2: Rand[(Double, Int)] = product(nonNegativeDouble, nonNegative)

  def replicateM(n: Int): Rand[List[Int]] = sequence(List.fill(n)(int))

  def nonNegativeLessThan(n: Int): Rand[Int] = map(nonNegative)(i => i % n)

  def nonNegativeLessThan2(n: Int): Rand[Int] = rng => {
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) >= mod) (mod, rng2)
    else nonNegativeLessThan2(n)(rng)
  }

  def nonNegativeLessThan3(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n - 1) >= mod) unit(mod)
      else nonNegativeLessThan3(n)
    })

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(i => i + 1)

  // primitives
  def unit[A](a: A): Rand[A] = (rng: RNG) => (a, rng)

  def flatMap[A, B](s: Rand[A])(f: A => Rand[B]): Rand[B] = (rng: RNG) => {
    val (a, rng2) = s(rng)
    f(a)(rng2)
  }

  // derived
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => theRng => (f(a), theRng))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a, b)))

  def product[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((rand, acc) => map2(rand, acc)((a, ls) => a :: ls))

}

