package net.tremman.scala.playground.functors

import net.tremman.scala.playground.monads.Functor

import scala.language.higherKinds

trait Applicative[F[_]] extends Functor[F] {

  // primitive
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

  def unit[A](a: => A): F[A]

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  // derived

  // in terms of map2
  def _apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fa, fab)((a: A, f: A => B) => f(a))

  // in terms of apply and unit
  def _map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {

    val fab: F[((A, B)) => F[C]] = unit((t: (A, B)) => unit(f(t._1, t._2)))

    null
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit[Unit](()))((a: A, _: Unit) => f(a))

  def sequence[A](fal: List[F[A]]): F[List[A]] =
    fal.foldRight(unit(List[A]()))((fa, fla) => map2(fa, fla)(_ :: _))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, flb) => map2(f(a), flb)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))
}
