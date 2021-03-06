package net.tremman.scala.playground.monads

import net.tremman.scala.playground.error.Either
import net.tremman.scala.playground.functors.Applicative
import net.tremman.scala.playground.parallel.Par
import net.tremman.scala.playground.parallel.Par.Par
import net.tremman.scala.playground.state.State
import net.tremman.scala.playground.stream.Cons
import net.tremman.scala.playground.test.property.Gen
import net.tremman.scala.playground.{error, stream}

import scala.language.{higherKinds, implicitConversions}

// all monads are functors but all functors are not monads
trait Monad[F[_]] extends Applicative[F] {

  // primitive
  def unit[A](a: => A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def _flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = compose((_: Unit) => fa, f)(fa)

  // derived
  override def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fa, fab)((a, f) => f(a))

  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(b => g(b))

  def _compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = (a: A) => join(map(f(a))((b: B) => g(b)))

  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  override def sequence[A](ls: List[F[A]]): F[List[A]] =
    ls.foldRight(unit(List[A]()))((fa: F[A], fla: F[List[A]]) => {
      println("Loop into list of F[A]s, F[A]: " + fa + ", F[List[A]]: " + fla)
      map2(fa, fla)((a: A, la: List[A]) => {
        println("Inner loop, A elem: " + a + ", list of As: " + la)
        a :: la
      })
    })

  override def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a: A, acc: F[List[B]]) =>
      map2(f(a), acc)((b: B, ls: List[B]) => b :: ls))

  override def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  def doWhile[A](a: F[A])(cond: A => F[Boolean]): F[Unit] = for {
    a1 <- a
    ok <- cond(a1)
    _ <- if (ok) doWhile(a)(cond) else unit(())
  } yield ()

  def forever[A, B](a: F[A]): F[B] = {
    lazy val t: F[B] = forever(a)
    a.flatMap(_ => t)
  }

  def foldM[A, B](l: stream.Stream[A])(z: B)(f: (B, A) => F[B]): F[B] = l match {
    case Cons(h, t) => f(z, h()) flatMap (z2 => foldM(t())(z2)(f))
    case _ => unit(z)
  }

  def as[A, B](a: F[A])(b: B): F[B] = map(a)(_ => b)

  def skip[A](a: F[A]): F[Unit] = as(a)(())

  def foldM_[A, B](l: stream.Stream[A])(z: B)(f: (B, A) => F[B]): F[Unit] = skip(foldM(l)(z)(f))

  def foreachM[A](l: stream.Stream[A])(f: A => F[Unit]): F[Unit] = foldM_(l)(())((_, a) => skip(f(a)))

  implicit def asMonadic[A](a: F[A]): Monadic[F, A] =
    new Monadic[F, A] {
      override val F: Monad[F] = Monad.this

      override def get: F[A] = a
    }

}

trait Monadic[F[_], A] {
  val F: Monad[F]

  def get: F[A]

  private val a = get

  def map[B](f: A => B): F[B] = F.map(a)(f)

  def flatMap[B](f: A => F[B]): F[B] = F.flatMap(a)(f)
}

object Monad {

  object GenMonad extends Monad[Gen] {
    override def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] = fa.flatMap(f)
  }

  object ParMonad extends Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(fa)(f)
  }

  object OptionMonad extends Monad[error.Option] {
    override def unit[A](a: => A): error.Option[A] = error.Some(a)

    override def flatMap[A, B](fa: error.Option[A])(f: A => error.Option[B]): error.Option[B] = fa.flatMap(f)
  }

  object StreamMonad extends Monad[stream.Stream] {
    override def unit[A](a: => A): stream.Stream[A] = stream.Stream.constant(a)

    override def flatMap[A, B](fa: stream.Stream[A])(f: A => stream.Stream[B]): stream.Stream[B] = fa.flatMap(f)
  }

  object ListMonad extends Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
  }

  class StateMonads[S] {
    type StateS[A] = State[S, A]

    object StateMonad extends Monad[StateS] {
      override def unit[A](a: => A): State[S, A] = State(s => (a, s))

      override def flatMap[A, B](fa: StateS[A])(f: A => StateS[B]): StateS[B] = fa.flatMap(f)
    }

  }

  // a State object whose state type is Int. The type of the outcome can be whatever we want
  object IntStateMonad extends Monad[({type IntState[E] = State[Int, E]})#IntState] {
    override def unit[A](a: => A): State[Int, A] = State(s => (a, s))

    override def flatMap[A, B](fa: State[Int, A])(f: A => State[Int, B]): State[Int, B] = fa.flatMap(f)
  }

  // a stateMonad is parameterized on the type of the state (S) and then on the type of the outcome (E)
  def stateMonad[S]: Monad[({type f[x] = State[S, x]})#f] = new Monad[({type f[x] = State[S, x]})#f] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = fa.flatMap(f)
  }

  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = net.tremman.scala.playground.error.Right(a)

    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa.flatMap(f)
  }

}
