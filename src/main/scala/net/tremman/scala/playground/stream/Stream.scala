package net.tremman.scala.playground.stream

import net.tremman.scala.playground.stream.Stream.unfold

import scala.annotation.tailrec

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def headOptionWithFoldRight: Option[A] = {
    foldRight(None: Option[A])((a, b) => if (b.isEmpty) Some(a) else None)
  }

  def toList: List[A] = {
    @tailrec
    def go(s: Stream[A], accumulator: List[A]): List[A] = s match {
      case Empty => accumulator
      case Cons(h, t) => go(t(), h() :: accumulator)
    }

    go(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
    case Empty => Stream.empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Stream.empty
  }

  def takeWhileWithFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty: Stream[A])((a, b) => if (p(a))
      Stream.cons(a, b) else
      Stream.empty)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def append[B >: A](anotherStream: Stream[B]): Stream[B] =
    foldRight(anotherStream)((a, b) => Stream.cons(a, b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty: Stream[A])((a, b) =>
      if (p(a)) Stream.cons(a, b.filter(p)) else b.filter(p))

  def find(p: A => Boolean): Option[A] = filter(p).headOption

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty: Stream[B])((a, b) => Stream.cons(f(a), b))

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold(this) {
    case Cons(h, t) if n > 0 => Some((h(), t().takeViaUnfold(n - 1)))
    case _ => None
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = {
    var theConstantStream: Stream[A] = null;
    theConstantStream = cons(a, theConstantStream)
    theConstantStream
  }

  def from(start: Int): Stream[Int] = {
    def funcStream(theNumber: Int): Stream[Int] = cons(theNumber, funcStream(theNumber + 1))

    funcStream(start)
  }

  def fibs(): Stream[Int] = {
    def fibonacci(n: Int): Int = {
      def go(numberIdx: Int): Int = numberIdx match {
        case 0 => 0
        case 1 => 1
        case _ => go(numberIdx - 1) + go(numberIdx - 2)
      }

      go(n)
    }

    def funcStream(numberIdx: Int): Stream[Int] =
      cons(fibonacci(numberIdx), funcStream(numberIdx + 1))

    funcStream(0)
  }

  def unfold[A, S](seed: S)(f: S => Option[(A, S)]): Stream[A] =
    f(seed) match {
      case Some((nextValue, nextSeed)) => cons(nextValue, unfold(nextSeed)(f))
      case _ => empty
    }

  def zipWith[A, B, C](one: Stream[A], another: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold((one, another)) {
      case (Cons(h, t), Cons(j, y)) => Some((f(h(), j()), (t(), y())))
      case _ => None
    }
  }

  def fibsViaUnfold(): Stream[Int] = unfold((0, 1))(
    (s: (Int, Int)) => s match {
      case (f0, f1) => Some(f0, (f1, f0 + f1))
    }
  )

  def fromViaUnfold(start: Int): Stream[Int] = unfold(start)(n => Some(n, n + 1))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  def onesViaUnfold(): Stream[Int] = unfold(1)(_ => Some(1, 1))

}
