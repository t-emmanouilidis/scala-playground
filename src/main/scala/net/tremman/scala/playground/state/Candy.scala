package net.tremman.scala.playground.state

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def update: Input => Machine => Machine = (theInput: Input) => (theMachine: Machine) =>
    (theInput, theMachine) match {
      case (_, Machine(_, 0, _)) => theMachine
      case (Turn, Machine(true, _, _)) => theMachine
      case (Coin, Machine(false, _, _)) => theMachine
      case (Turn, Machine(false, candies, coins)) => Machine(locked = true, candies - 1, coins)
      case (Coin, Machine(true, candies, coins)) => Machine(locked = false, candies, coins + 1)
    }
}
