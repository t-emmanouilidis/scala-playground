package net.tremman.scala.playground.monads

import net.tremman.scala.playground.error.{None, Option, Some}
import org.scalatest.FunSuite

class MonadTest extends FunSuite {

  test("should be able to replicate a list monad") {
    val listMonad = Monad.ListMonad

    val lss: List[List[Int]] = listMonad.replicateM(2, List(1, 2))

    println(lss)
  }

  test("should be able to replicate an option monad") {
    val optionMonad = Monad.OptionMonad

    val opp: Option[List[Int]] = optionMonad.replicateM(2, Some(1))

    println(opp)
  }

  test("should be able to replicate the option monad") {
    val result = Monad.OptionMonad.replicateM(2, Some(1))

    assert(result === Some(List(1, 1)))
  }

  test("should be able to replicate the list monad") {
    val result = Monad.ListMonad.replicateM(2, List(1, 2))

    assert(result === List(List(1, 1), List(1, 2), List(2, 1), List(2, 2)))
  }

  test("flatMap associativity for option monad") {
    val f: Int => Option[Int] = i => Some(i + 1)
    val g: Int => Option[Int] = i => Some(i + 2)

    assert(None.flatMap(f).flatMap(g) === None.flatMap(f(_).flatMap(g)))

    assert(Some(1).flatMap(f).flatMap(g) === Some(1).flatMap(f(_).flatMap(g)))
  }

  test("compose associativity") {
    val f: Int => Option[String] = i => Some(i.toString)
    val g: String => Option[String] = str => Some(str + "2")
    val h: String => Option[Int] = str => Some(Integer.parseInt(str))

    import Monad.OptionMonad._
    assert(compose(compose(f, g), h)(1) === compose(f, compose(g, h))(1))
  }

  test("compose identity for option") {
    val f: Int => Option[Int] = i => Some(i * 2)
    import Monad.OptionMonad.{compose, flatMap}
    val unit: Int => Option[Int] = i => Monad.OptionMonad.unit(i)

    assert(compose(f, unit)(2) === f(2))
    assert(compose(f, unit)(2) === compose(unit, f)(2))

    assert(flatMap(Some(1))(unit) === Some(1))
    assert(flatMap(unit(1))(f) === f(1))
  }

  test("compose identity for list") {
    val f: Int => List[Int] = i => List(i * 2)
    import Monad.ListMonad.{compose, flatMap}
    val unit: Int => List[Int] = i => Monad.ListMonad.unit(i)

    assert(compose(f, unit)(2) === f(2))
    assert(compose(f, unit)(2) === compose(unit, f)(2))

    assert(flatMap(List(2))(unit) === List(2))
    assert(flatMap(unit(2))(f) === f(2))
  }

  test("int state monad") {
    val F = Monad.stateMonad[Int]

    def zipWithIndex[A](as: List[A]): List[(Int, A)] =
      as.foldLeft(F.unit(List[(Int, A)]()))((acc, a) => for {
        xs <- acc
        n <- getState
        _ <- setState(n + 1)
      } yield (n, a) :: xs).run(0)._1.reverse
  }

}
