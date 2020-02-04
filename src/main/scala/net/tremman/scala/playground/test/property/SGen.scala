package net.tremman.scala.playground.test.property

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] = SGen(forSize(_).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val tempGen: Int => Gen[B] = n => {
      forSize(n).flatMap((a: A) => f(a).forSize(n))
    }
    SGen(tempGen)
  }

  def **[B](s2: SGen[B]): SGen[(A, B)] = SGen(n => apply(n).**(s2(n)))
}
