package net.tremman.scala.playground.effects.external

import net.tremman.scala.playground.effects.external.Effects.IO

case class Player(name: String, score: Int) {
}

object Player {

  def contest(one: Player, another: Player): IO[Unit] =
    Effects.printLine(winnerMsg(winner(one, another)))

  def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score) Some(p1)
    else if (p2.score > p1.score) Some(p2)
    else None

  def winnerMsg(p: Option[Player]): String = p map {
    case Player(name, _) => s"${name} is the winner"
  } getOrElse "It's a draw"

}
