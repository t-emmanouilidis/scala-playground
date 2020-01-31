package net.tremman.scala.playground.state

import org.scalatest.FunSuite

class StateTest extends FunSuite {

  test("rollDie") {
    assert(1 === RNG.rollDie(RNG.SimpleRNG(5))._1)
  }

}
