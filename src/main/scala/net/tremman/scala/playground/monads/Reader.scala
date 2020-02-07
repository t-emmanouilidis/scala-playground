package net.tremman.scala.playground.monads

case class Reader[R, A](run: R => A)

object Reader {

  def readerNomad[R] = new Monad[({type F[E] = Reader[R, E]})#F] {
    override def unit[A](a: => A): Reader[R, A] = ???

    override def flatMap[A, B](fa: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = ???
  }

}
