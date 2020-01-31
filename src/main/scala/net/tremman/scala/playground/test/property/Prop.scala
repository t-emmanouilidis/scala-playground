package net.tremman.scala.playground.test.property

import java.util.concurrent.Executors

import net.tremman.scala.playground.parallel.Par.Par
import net.tremman.scala.playground.state.{RNG, State}
import net.tremman.scala.playground.test.property.Prop.{FailedCase, MaxSize, SuccessCount, TestCases}

import language.implicitConversions

case class Gen[+A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap((a: A) => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen.listOfN(n, this))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  def unsized: SGen[A] = SGen(_ => this)

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def map2[B, C](anotherGen: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(anotherGen.sample)(f))

  def **[B](anotherGen: Gen[B]): Gen[(A, B)] = (this map2 anotherGen) ((_, _))
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegative).map(n => start + (n % (stopExclusive - start))))

  def choosePair(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    Gen(listOfN(2, choose(start, stopExclusive)).sample.map(ls => (ls.head, ls.tail.head)))

  def unit[A](a: => A): Gen[A] = Gen(State(r => (a, r)))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def string: Gen[String] = Gen(listOfN(10, choose(0, 10)).sample.map(ls => ls.mkString))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(d => if (d < threshold) g1._1.sample else g2._1.sample))
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n))

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n.max(1)))

  def genStringIntFn(g: Gen[Int]): Gen[String => Int] = g.map((i: Int) => (s: String) => i)

  implicit def unsized[A](g: Gen[A]): SGen[A] = SGen(_ => g)

  object ** {
    def unapply[A, B](p: (A, B)): Some[(A, B)] = Some(p)
  }

}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] = SGen(forSize(_).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val tempGen: Int => Gen[B] = n => {
      forSize(n).flatMap((a: A) => f(a).forSize(n))
    }
    SGen(tempGen)
  }

  def **[B](s2: SGen[B]): SGen[(A, B)] = SGen(n => apply(n).**(s2(n)))
}

sealed trait Result {
  def isFailure: Boolean
}

case object Passed extends Result {
  override def isFailure = false
}

case class Failed(failure: FailedCase, successes: SuccessCount) extends Result {
  override def isFailure: Boolean = true
}

case object Proved extends Result {
  override def isFailure: Boolean = true
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop(
    (max: MaxSize, n: TestCases, rng: RNG) => run(max, n, rng) match {
      case Passed | Proved => p.run(max, n, rng)
      case failed => failed
    }
  )

  def ||(p: Prop): Prop = Prop(
    (max: MaxSize, n: TestCases, rng: RNG) => run(max, n, rng) match {
      case Failed(_, _) => p.run(max, n, rng)
      case x => x
    }
  )

}

object Prop {
  type MaxSize = Int
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll((n: Int) => g.forSize(n))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop((max, n, rng) => {
    val casesPerSize = (n + (max - 1)) / max
    val props: Stream[Prop] = Stream.from(0).take((n.min(max)) + 1).map(i => forAll(g(i))(f))
    val prop: Prop = props.map(p => Prop((max, _, rng) => p.run(max, casesPerSize, rng))).toList.reduce(_ && _)
    prop.run(max, n, rng)
  })

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop((max, n, rng) =>
    randomStream(gen)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Failed(a.toString, i)
      } catch {
        case ex: Exception => Failed(buildErrorMsg(a, ex), i)
      }
    }.find(aResult => aResult.isFailure).getOrElse(Passed))

  def randomStream[A](theGenerator: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(theGenerator.sample.run(rng)))

  def buildErrorMsg[A](a: A, ex: Exception): String =
    s"test case: $a\n" +
      s"generated an exception: ${ex.getMessage}\n" +
      s"stack trace:\n ${ex.getStackTrace.mkString("\n")}"

  def run(p: Prop,
          maxSize: MaxSize = 100,
          testCases: TestCases = 100,
          rng: RNG = RNG.SimpleRNG(System.currentTimeMillis())): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Failed(msg, successes) =>
        println(s"! Failed after $successes passed tests:${System.lineSeparator()}$msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }
  }

  def check(p: => Boolean): Prop = Prop((_, _, _) => {
    if (p) Passed else Failed("()", 0)
  })

  private val ES = Gen.weighted(Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> 0.75,
    Gen.unit(Executors.newCachedThreadPool) -> 0.25)

  object ** {
    def unapply[A, B](p: (A, B)): Some[(A, B)] = Some(p)
  }

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(ES ** g) {
      case es ** a => f(a)(es).get()
    }

  def checkPar(p: Par[Boolean]): Prop = forAllPar(Gen.unit(()))(_ => p)

}
