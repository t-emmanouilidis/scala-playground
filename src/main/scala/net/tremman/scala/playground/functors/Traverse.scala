package net.tremman.scala.playground.functors

import net.tremman.scala.playground.error.{None, Option, Some}
import net.tremman.scala.playground.monads.{Functor, Monad}
import net.tremman.scala.playground.state.State

import scala.language.higherKinds

trait Traverse[F[_]] extends Functor[F] {
  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = sequence(map(fa)(f))

  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)((ga: G[A]) => ga)

  type Id[A] = A

  val identityApplicative: Applicative[Id] = new Applicative[Id] {
    override def apply[A, B](fab: Id[A => B])(fa: Id[A]): Id[B] = fab.apply(fa)

    override def unit[A](a: => A): Id[A] = a
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(f)(identityApplicative)

  def traverseState[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseState(fa)((a: A) => for {
      s1 <- State.get[S]
      (b, s2) = f(a, s1)
      _ <- State.set(s2)
    } yield b).run(s)

  def zipWithIndex[A](ta: F[A]): F[(A, Int)] =
    traverseState(ta)((a: A) => for {
      i <- State.get[Int]
      _ <- State.set(i + 1)
    } yield (a, i)).run(0)._1

  def _zipWithIndex[A](ta: F[A]): F[(A, Int)] = mapAccum(ta, 0)((a, s) => ((a, s), s + 1))._1

  def toList[A](ta: F[A]): List[A] =
    traverseState(ta)((a: A) => for {
      as <- State.get[List[A]]
      _ <- State.set(a :: as)
    } yield ()).run(Nil)._2.reverse

  def _toList[A](ta: F[A]): List[A] = mapAccum(ta, List[A]())((a, s) => ((), a :: s))._2.reverse

  def reverse[A](fa: F[A]): F[A] = mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  def foldLeft[A, B](fa: F[A])(z: F[B])(f: (B, A) => B): F[B] = ???

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G product H)

}

case class Tree[+A](head: A, tail: List[Tree[A]])

object Traverse {

  def listTraversable: Traverse[List] = new Traverse[List] {
    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(G.unit(List[B]()))((a: A, glb: G[List[B]]) => G.map2(f(a), glb)((a: B, lb: List[B]) => a :: lb))
  }

  def optionTraversable: Traverse[Option] = new Traverse[Option] {
    override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      fa match {
        case Some(a) => G.map(f(a))(Some(_))
        case None => G.unit(None)
      }
  }

  def treeTraversable: Traverse[Tree] = new Traverse[Tree] {
    override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(f(fa.head), listTraversable.traverse(fa.tail)((a: Tree[A]) => traverse(a)(f)))((b: B, gl: List[Tree[B]]) => Tree(b, gl))

  }

}


