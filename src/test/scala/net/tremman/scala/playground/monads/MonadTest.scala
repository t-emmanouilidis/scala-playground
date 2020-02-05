package net.tremman.scala.playground.monads

import net.tremman.scala.playground.error
import net.tremman.scala.playground.error.Some
import org.scalatest.FunSuite

class MonadTest extends FunSuite {

  test("should be able to replicate a list monad") {
    val listMonad = Monad.listMonad

    val lss: List[List[Int]] = listMonad.replicateM(2, List(1,2))

    println(lss)
  }

  test("should be able to replicate an option monad") {
    val optionMonad = Monad.optionMonad

    val opp: error.Option[List[Int]] = optionMonad.replicateM(2, Some(1))

    println(opp)
  }

}
