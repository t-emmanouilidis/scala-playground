package net.tremman.scala.playground.test.property

import java.util.concurrent.Executors

import net.tremman.scala.playground.parallel.Par.Par
import net.tremman.scala.playground.state.RNG
import net.tremman.scala.playground.test.property.Prop.{FailedCase, MaxSize, SuccessCount, TestCases}

import scala.language.implicitConversions

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
    Stream.unfold(rng)(rng => Some(theGenerator.state.run(rng)))

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
