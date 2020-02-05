package net.tremman.scala.playground.monads

import net.tremman.scala.playground.parallel.Par
import net.tremman.scala.playground.parallel.Par.Par
import net.tremman.scala.playground.state.State
import net.tremman.scala.playground.test.property.Gen
import net.tremman.scala.playground.{error, stream}

import scala.language.higherKinds

// all monads are functors but all functors are not monads
trait Monad[F[_]] extends Functor[F] {

  // primitive
  def unit[A](a: => A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  // derived
  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def sequence[A](ls: List[F[A]]): F[List[A]] =
    ls.foldRight(unit(List[A]()))((fa: F[A], fal: F[List[A]]) =>
      map2(fa, fal)((a: A, la: List[A]) => a :: la))

  def traverse[A, B](ls: List[A])(f: A => F[B]): F[List[B]] =
    ls.foldRight(unit(List[B]()))((a: A, flb: F[List[B]]) =>
      map2(f(a), flb)((b: B, lb: List[B]) => b :: lb))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

}

object Monad {

  val genMonad: Monad[Gen] = new Monad[Gen] {
    override def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] = fa.flatMap(f)
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(fa)(f)
  }

  val optionMonad: Monad[error.Option] = new Monad[error.Option] {
    override def unit[A](a: => A): error.Option[A] = error.None

    override def flatMap[A, B](fa: error.Option[A])(f: A => error.Option[B]): error.Option[B] = fa.flatMap(f)
  }

  val streamMonad: Monad[stream.Stream] = new Monad[stream.Stream] {
    override def unit[A](a: => A): stream.Stream[A] = stream.Stream.constant(a)

    override def flatMap[A, B](fa: stream.Stream[A])(f: A => stream.Stream[B]): stream.Stream[B] = fa.flatMap(f)
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
  }

  class StateMonads[S] {
    type StateS[A] = State[S, A]

    val stateMonad: Monad[StateS] = new Monad[StateS] {
      override def unit[A](a: => A): State[S, A] = State(s => (a, s))

      override def flatMap[A, B](fa: StateS[A])(f: A => StateS[B]): StateS[B] = fa.flatMap(f)
    }
  }

}
