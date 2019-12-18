package net.tremman.scala.sample.error

object SeqUtils {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.foldLeft(0.0)(_ + _) / xs.size)

  def variance(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else mean(xs).flatMap(m => mean(xs.map(x => Math.pow(x - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

}
