package net.tremman.scala.playground.functors

import net.tremman.scala.playground.error.{None, Option, Some}
import net.tremman.scala.playground.monads.Functor

import scala.language.higherKinds

trait Traverse[F[_]] extends Functor[F] {
  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = sequence(map(fa)(f))

  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)((ga: G[A]) => ga)

  def map[A, B](fa: F[A])(f: A => B): F[B] = ???
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


