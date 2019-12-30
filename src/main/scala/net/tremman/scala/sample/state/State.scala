package net.tremman.scala.sample.state

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRng) = rng.nextInt

    (if (n < 0) -(n + 1) else n, nextRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRng) = nonNegativeInt(rng)
    val d: Double = n.toDouble / Int.MaxValue
    (d, nextRng)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, nextRng) = rng.nextInt
    val (d, nextRng2) = double(nextRng)
    ((n, d), nextRng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((n, d), nextRng) = intDouble(rng)
    ((d, n), nextRng)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(currentCount: Int, currentRng: RNG, theInts: List[Int]): (List[Int], RNG) = {
      if (currentCount > 0) {
        val (n, rng2) = currentRng.nextInt
        go(currentCount - 1, rng2, n :: theInts)
      } else
        (Nil, rng)
    }

    go(count, rng, List())
  }

  type Rand[A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = (rng: RNG) => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegative: Rand[Int] = map(nonNegativeInt)(n => n)

  def nonNegativeEven: Rand[Int] = map(nonNegative)(i => i - i % 2)

  def boolean: Rand[Boolean] = map(nonNegative)(i => i % 2 == 0)

  def double2: Rand[Double] = map(nonNegative)(n => n.toDouble)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val intDouble2: Rand[(Int, Double)] = both(int, double2)

  val doubleInt2: Rand[(Double, Int)] = both(double2, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((rand, acc) =>
      map2(rand, acc)((a, ls) => a :: ls))

  def ints2(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def nonNegativeLessThan(n: Int): Rand[Int] = map(nonNegative)(i => i % n)

  def nonNegativeLessThan2(n: Int): Rand[Int] = rng => {
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) >= mod) (mod, rng2)
    else nonNegativeLessThan2(n)(rng)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan3(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n - 1) >= mod) unit(mod)
      else nonNegativeLessThan3(n)
    })

  def map_2[A, B](x: Rand[A])(f: A => B): Rand[B] = flatMap(x)(a => unit(f(a)))

  def map2_2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map_2(rb)(b => f(a, b)))

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(i => i + 1)

}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    @tailrec
    def go(s: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) =
      actions match {
        case Nil => (acc.reverse, s)
        case h :: t => h.run(s) match {
          case (a, s2) => go(s2, t, a :: acc)
        }
      }

    State(s => go(s, sas, List()))
  }

  def sequenceViaFoldLeft[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)((a, ls) => a :: ls))

  def get[S]: State[S, S] = State(s => (s, s))

}