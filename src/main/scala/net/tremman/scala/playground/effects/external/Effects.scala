package net.tremman.scala.playground.effects.external

import net.tremman.scala.playground.effects.external.Effects.IO.map2
import net.tremman.scala.playground.monads.{Monad, Monadic}

import scala.annotation.tailrec
import scala.io.StdIn

object Effects {

  sealed trait IO[A] extends Monadic[IO, A] {
    override val F: Monad[IO] = IO

    override def get: IO[A] = this
  }

  // IO action that has finished. just returns the result
  case class Return[A](a: A) extends IO[A]

  // effect that produces a result
  case class Suspend[A](resume: () => A) extends IO[A]

  case class FlatMap[A, B](io: IO[A], f: A => IO[B]) extends IO[B]

  object IO extends Monad[IO] {

    override def unit[A](a: => A): IO[A] = Return(a)

    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = FlatMap(fa, f)

    override def map[A, B](fa: IO[A])(f: A => B): IO[B] = flatMap(fa)(f.andThen(Return(_)))

    def suspend[A](a: => IO[A]): IO[A] = Suspend(() => ()).flatMap(_ => a)
  }

  // identity value
  val empty: IO[Unit] = Return(())

  // the Suspend constructor just want a supplier of something,
  // here we have a supplier of an IO instance
  def printLine(msg: String): IO[Unit] = Suspend(() => Return(println(msg)))

  def readLine: IO[String] = Suspend(() => StdIn.readLine)

  def echo: IO[Unit] = readLine.flatMap(printLine)

  def readInt: IO[Int] = readLine.map(_.toInt)

  def readInts: IO[(Int, Int)] = map2(readInt, readInt)((_, _))

  @tailrec
  def run[A](io: IO[A]): Any = {
    type F = Any => IO[Any]
    io match {
      case Return(a) => a
      case Suspend(r) => r()
      case FlatMap(x, f: F) => x match {
        case Return(a) => run(f(a))
        case Suspend(r) => run(f(r()))
        case FlatMap(y, g: F) => run(y.flatMap(a => g(a).flatMap(f)))
      }
    }
  }

  def main(args: Array[String]): Unit = {
    run(IO.forever(printLine("Still going...")))
  }

}
