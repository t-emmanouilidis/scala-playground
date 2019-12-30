package net.tremman.scala.sample.test.property

import net.tremman.scala.sample.state.{RNG, State}
import net.tremman.scala.sample.test.property.Prop.{FailedCase, SuccessCount, TestCases}

case class Gen[A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap((a: A) => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen.listOfN(n, this))
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

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =

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

case class Prop(run: (TestCases, RNG) => Result) {
  def check: Result = ???

  def &&(p: Prop): Prop = {
    new Prop() {
      override def check: Result = None
    }
  }

}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type Result = Option[(FailedCase, SuccessCount)]

  def forAll[A](theGenerator: Gen[A])(f: A => Boolean): Prop = Prop((n, rng) =>
    randomStream(theGenerator)(rng).zip(Stream.from(0)).take(n).map {
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

}
