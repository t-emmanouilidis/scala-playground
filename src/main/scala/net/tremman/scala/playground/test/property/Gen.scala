package net.tremman.scala.playground.test.property

import net.tremman.scala.playground.state.{RNG, State}

case class Gen[+A](state: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(state.flatMap((a: A) => f(a).state))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen.listOfN(n, this))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  def unsized: SGen[A] = SGen(_ => this)

  def map[B](f: A => B): Gen[B] = Gen(state.map(f))

  def map2[B, C](anotherGen: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(state.map2(anotherGen.state)(f))

  def **[B](anotherGen: Gen[B]): Gen[(A, B)] = (this map2 anotherGen) ((_, _))
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegative).map(n => start + (n % (stopExclusive - start))))

  def choosePair(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    Gen(listOfN(2, choose(start, stopExclusive)).state.map(ls => (ls.head, ls.tail.head)))

  def unit[A](a: => A): Gen[A] = Gen(State(r => (a, r)))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.state)))

  def string: Gen[String] = Gen(listOfN(10, choose(0, 10)).state.map(ls => ls.mkString))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(d => if (d < threshold) g1._1.state else g2._1.state))
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n))

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n.max(1)))

  def genStringIntFn(g: Gen[Int]): Gen[String => Int] = g.map((i: Int) => (s: String) => i)

  implicit def unsized[A](g: Gen[A]): SGen[A] = SGen(_ => g)

  object ** {
    def unapply[A, B](p: (A, B)): Some[(A, B)] = Some(p)
  }

}
