package net.tremman.scala.playground.monads

import net.tremman.scala.playground.error.{None, Option, Some}
import org.scalatest.FunSuite

class MonadTest extends FunSuite {

  test("should be able to replicate the option monad") {
    val result = Monad.optionMonad.replicateM(2, Some(1))

    assert(result === Some(List(1, 1)))
  }

  test("should be able to replicate the list monad") {
    val result = Monad.listMonad.replicateM(2, List(1, 2))

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

    import Monad.optionMonad._
    assert(compose(compose(f, g), h)(1) === compose(f, compose(g, h))(1))
  }

  test("compose identity") {
    val f: Int => Option[Int] = i => Some(i * 2)
    import Monad.optionMonad.{compose, flatMap}
    val unit: Int => Option[Int] = i => Monad.optionMonad.unit(i)
    assert(compose(f, unit)(2) === f(2))
    assert(compose(f, unit)(2) === compose(unit, f)(2))

    assert(flatMap(Some(1))(unit) === Some(1))
    assert(flatMap(unit(1))(f) === f(1))
  }


}
