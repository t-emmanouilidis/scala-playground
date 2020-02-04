package net.tremman.scala.playground.monoids

trait WordCount {
}

case class Stub(chars: String) extends WordCount

case class Part(leftStub: String, words: Int, rightStub: String) extends WordCount

object WordCount {
  val wordCountMonoid: Monoid[WordCount] = new Monoid[WordCount] {
    override def op(a1: WordCount, a2: WordCount): WordCount = (a1, a2) match {
      case (Stub(chars1), Stub(chars2)) => Stub(chars1 + chars2)
      case (Stub(chars), Part(left, words, right)) => Part(chars + left, words, right)
      case (Part(left, words, right), Stub(chars)) => Part(left, words, right + chars)
      case (Part(left1, words1, right1), Part(left2, words2, right2)) =>
        Part(left1, words1 + (if ((right1 + left2).isEmpty) 0 else 1) + words2, right2)
    }

    override def zero: WordCount = Stub("")
  }

  def count(s: String): Int = {
    def wc(c: Char): WordCount =
      if (c.isWhitespace) Part("", 0, "") else Stub(c.toString)

    def unStub(s: String) = s.length min 1

    Monoid.foldMapV(s.toList, wordCountMonoid)(wc) match {
      case Stub(s) => unStub(s)
      case Part(left, words, right) => unStub(left) + words + unStub(right)
    }
  }

}
