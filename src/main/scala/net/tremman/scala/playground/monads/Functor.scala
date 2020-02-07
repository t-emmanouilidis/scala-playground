package net.tremman.scala.playground.monads

import scala.language.higherKinds

trait Functor[F[_]] {

  // primitive
  def map[A, B](fa: F[A])(f: A => B): F[B]

  // derived
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))

  def coDistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }

}

object Functor {

  val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  object Laws {
    // empty for now
  }

}
