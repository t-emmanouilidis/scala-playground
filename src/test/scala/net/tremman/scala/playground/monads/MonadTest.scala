package net.tremman.scala.playground.monads

import net.tremman.scala.playground.error.Some
import org.scalatest.FunSuite

class MonadTest extends FunSuite {

  test("should be able to replicate the option monad") {
    val result = Monad.optionMonad.replicateM(2, Some(1))

    assert(result === Some(List(1, 1)))
  }

  test("should be able to replicate the list monad") {
    val result = Monad.listMonad.replicateM(2, List(1, 2))

    assert(result === List(List(1,1), List(1,1)))
  }

}
