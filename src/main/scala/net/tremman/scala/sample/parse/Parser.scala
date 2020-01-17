package net.tremman.scala.sample.parse

trait ParseError {

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

  implicit def operators[A](theParser: Parser[A]): ParserOps[A] = ParserOps(theParser)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](one: Parser[A]) {
    def |[B >: A](another: Parser[B]): Parser[B] = self.or(one, another)

    def or[B >: A](another: => Parser[B]): Parser[B] = self.or(one, another)
  }

}

object Parsers {

}
