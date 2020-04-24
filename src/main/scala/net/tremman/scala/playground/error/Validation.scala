package net.tremman.scala.playground.error

import net.tremman.scala.playground.functors.Applicative

sealed trait Validation[+E, +A] {

}

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object Validation {

  def validationApplicative[E] = new Applicative[({type f[A] = Validation[E, A]})#f] {
    override def apply[A, B](fab: Validation[E, A => B])(fa: Validation[E, A]): Validation[E, B] = fa match {
      case Failure(h, t) => Failure(h, t)
      case Success(a) => fab match {
        case Failure(h, t) => Failure(h, t)
        case Success(f) => Success(f(a))
      }
    }

    override def unit[A](a: => A): Validation[E, A] = Success(a)
  }

}
