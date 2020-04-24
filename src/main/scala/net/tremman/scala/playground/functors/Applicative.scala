package net.tremman.scala.playground.functors

import net.tremman.scala.playground.monads.Functor

import scala.language.higherKinds

trait Applicative[F[_]] extends Functor[F] {

  // primitive
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

  def unit[A](a: => A): F[A]

  // in terms of apply and unit
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val curried = f.curried
    val intermediate: F[B => C] = apply(unit(curried))(fa)
    apply(intermediate)(fb)
  }

  // derived

  // in terms of map2
  def _apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fa, fab)((a: A, f: A => B) => f(a))

  // 12.3
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    val curried = f.curried

    val x: F[B => C => D] = apply(unit(curried))(fa)
    val y: F[C => D] = apply(x)(fb)
    apply(y)(fc)
  }

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    val curried = f.curried

    val x: F[B => C => D => E] = apply(unit(curried))(fa)
    val y: F[C => D => E] = apply(x)(fb)
    val z: F[D => E] = apply(y)(fc)
    apply(z)(fd)
  }

  // in terms of map2
  def _map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit[Unit](()))((a: A, _: Unit) => f(a))

  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

  def sequence[A](fal: List[F[A]]): F[List[A]] =
    fal.foldRight(unit(List[A]()))((fa, fla) => map2(fa, fla)(_ :: _))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, flb) => map2(f(a), flb)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  // 12.8
  def product[G[_]](g: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] =
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def apply[A, B](fab: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) =
        (Applicative.this.map2(fab._1, fa._1)((fn, a) => fn(a)),
          g.map2(fab._2, fa._2)((gn, b) => gn(b)))

      override def unit[A](a: => A): (F[A], G[A]) = (Applicative.this.unit(a), g.unit(a))
    }

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = {
    ofa.foldRight(unit(Map[K, V]()))((theEntry: (K, F[V]), wrappedMap: F[Map[K, V]]) =>
      map2(theEntry._2, wrappedMap)((theValue: V, theMap: Map[K, V]) => theMap.+((theEntry._1, theValue))))
  }
}

object Applicative {

  def listApplicative: Applicative[List] = new Applicative[List] {
    override def apply[A, B](fab: List[A => B])(fa: List[A]): List[B] = List(fab.head.apply(fa.head))

    override def unit[A](a: => A): List[A] = List(a)
  }


}
