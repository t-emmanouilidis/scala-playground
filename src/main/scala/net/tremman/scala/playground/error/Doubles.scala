package net.tremman.scala.playground.error

object Doubles {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.foldLeft(0.0)(_ + _) / xs.size)

  def variance(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else mean(xs).flatMap(m => mean(xs.map(x => Math.pow(x - m, 2))))

}
