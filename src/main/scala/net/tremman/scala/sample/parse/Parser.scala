package net.tremman.scala.sample.parse

import net.tremman.scala.sample.test.property.{Gen, Prop}

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

  implicit def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  def orString(str1: String, str2: String): Parser[String]

  def or[A](one: Parser[A], another: Parser[A]): Parser[A]

  def listOfN[A](n: Int, aParser: Parser[A]): Parser[List[A]]

  def slice[A](p: Parser[A]): Parser[String]

  def map[A, B](p: Parser[A])(f: A => B): Parser[B]

  def product[A, B](one: Parser[A], another: Parser[B]): Parser[(A, B)]

  def map2[A, B, C](one: Parser[A], another: Parser[B])(f: (A, B) => C): Parser[C] =
    map(product(one, another)) {
      case (a, b) => f(a, b)
    }

  def zeroOrMore[A](aParser: Parser[A]): Parser[List[A]] = ???

  def oneOrMore[A](aParser: Parser[A]): Parser[List[A]] = map2(aParser, zeroOrMore(aParser))(_ :: _)

  def succeeds[A](a: A): Parser[A] = string("").map(_ => a)

  implicit def string(str: String): Parser[String]

  // this is the implicit function used to convert a Parser instance to a ParserOps instance
  implicit def operators[A](theParser: Parser[A]): ParserOps[A] = ParserOps(theParser)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](one: Parser[A]) {

    def map[B](f: A => B): Parser[B] = self.map(one)(f)

    def map2[B, C](another: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(one, another)(f)

    def slice(): Parser[String] = self.slice(one)

    def many(): Parser[List[A]] = self.many(one)

    def zeroOrMore(str: String): Parser[Int] = self.zeroOrMore(str)

    def oneOrMore(str: String): Parser[Int] = self.oneOrMore(str)

    def |[B >: A](another: Parser[B]): Parser[B] = self.or(one, another)

    def or[B >: A](another: => Parser[B]): Parser[B] = self.or(one, another)

    def product[B >: A](another: Parser[B]): Parser[(A, B)] = self.product(one, another)

    def **[B >: A](another: Parser[B]): Parser[(A, B)] = product(another)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(str => run(p1)(str) == run(p2)(str))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)

    def succeedLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(str => run(succeeds(str))("1") == Right(str))
  }

}

object Parsers {

}
