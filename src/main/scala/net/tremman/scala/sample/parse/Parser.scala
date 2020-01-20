package net.tremman.scala.sample.parse

trait ParseError {
  def msg: String
}

case class BasicParseError(msg: String) extends ParseError {
}

class Parser[A] {

}

trait Parsers[ParseError, Parser[+_]] {
  self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char]

  def orString(str1: String, str2: String): Parser[String]

  def or[A](one: Parser[A], another: Parser[A]): Parser[A]

  def listOfN[A](n: Int, aParser: Parser[A]): Parser[List[A]]

  implicit def string(str: String): Parser[String]

  // this is the implicit function used to convert a Parser instance to a ParserOps instance
  implicit def operators[A](theParser: Parser[A]): ParserOps[A] = ParserOps(theParser)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def many[A](p: Parser[A]): Parser[List[A]]

  def zeroOrMore(str: String): Parser[Int] = map(many(str))(_.size)

  def oneOrMore(str: String): Parser[Int]

  def followedBy[A](one: Parser[A], another: Parser[A]): Parser[A]

  def map[A, B](p: Parser[A])(f: A => B): Parser[B]

  case class ParserOps[A](one: Parser[A]) {
    def |[B >: A](another: Parser[B]): Parser[B] = self.or(one, another)

    def or[B >: A](another: => Parser[B]): Parser[B] = self.or(one, another)

    def followedBy[B >: A](another: Parser[B]): Parser[B] =
      self.followedBy(one, another)
  }

}

object Parsers {

}
