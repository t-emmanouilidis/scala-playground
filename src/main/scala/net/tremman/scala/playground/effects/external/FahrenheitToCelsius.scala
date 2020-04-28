package net.tremman.scala.playground.effects.external

import net.tremman.scala.playground.effects.external.Effects.IO

object FahrenheitToCelsius {

  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0

  def converter: IO[Unit] = for {
    _ <- Effects.printLine("Enter a temperature in Fahrenheit:")
    d <- Effects.readLine.map(_.toDouble)
    _ <- Effects.printLine(fahrenheitToCelsius(d).toString)
  } yield ()

}
