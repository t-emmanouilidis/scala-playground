package net.tremman.scala.playground.state

import org.scalatest.FunSuite

class StateTest extends FunSuite {

  test("rollDie") {
    assert(1 === RNG.rollDie(RNG.SimpleRNG(5))._1)
  }

  test("imperative code using state monad") {
    def concatAndPrint: State[String, Unit] =
      State.get.flatMap((theString: String) =>
        State.set(theString + "world!").flatMap(_ =>
          State.get.map(theUpdatedString => println(theUpdatedString))))

    assert(concatAndPrint.run("Hello, ")._2 === "Hello, world!")
  }

}
