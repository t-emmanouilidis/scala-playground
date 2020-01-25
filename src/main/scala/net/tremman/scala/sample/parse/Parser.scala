package net.tremman.scala.sample.parse

import net.tremman.scala.sample.test.property.{Gen, Prop}

import scala.util.matching.Regex

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

  // primitives
  def slice[A](p: Parser[A]): Parser[String]

  def or[A](one: Parser[A], another: Parser[A]): Parser[A]

  implicit def string(str: String): Parser[String]

  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  def flatMap[A, B](aParser: Parser[A])(f: A => Parser[B]): Parser[B]

  def regex(regex: Regex): Parser[String]

  // non primitives
  implicit def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  def orString(str1: String, str2: String): Parser[String] = or(string(str1), string(str2))

  def listOfN[A](n: Int, aParser: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(aParser, listOfN(n - 1, aParser))(_ :: _)

  def map[A, B](theParser: Parser[A])(f: A => B): Parser[B] = theParser.flatMap(f andThen succeed)

  def product[A, B](one: Parser[A], another: Parser[B]): Parser[(A, B)] = map2(one, another)((_, _))

  def map2[A, B, C](one: Parser[A], another: Parser[B])(f: (A, B) => C): Parser[C] =
    for {
      a <- one
      b <- another
    } yield f(a, b)

  def zeroOrMore[A](aParser: Parser[A]): Parser[List[A]] =
    map2(aParser, zeroOrMore(aParser))(_ :: _) or succeed(List())

  def oneOrMore[A](aParser: Parser[A]): Parser[List[A]] =
    map2(aParser, zeroOrMore(aParser))(_ :: _)

  // this is the implicit function used to convert a Parser instance to a ParserOps instance
  implicit def operators[A](theParser: Parser[A]): ParserOps[A] = ParserOps(theParser)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](theParser: Parser[A]) {

    def map[B](f: A => B): Parser[B] = self.map(theParser)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(theParser)(f)

    def map2[B, C](another: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(theParser, another)(f)

    def slice(): Parser[String] = self.slice(theParser)

    def zeroOrMore(aParser: Parser[A]): Parser[List[A]] = self.zeroOrMore(aParser)

    def oneOrMore(aParser: Parser[A]): Parser[List[A]] = self.oneOrMore(aParser)

    def |[B >: A](another: Parser[B]): Parser[B] = self.or(theParser, another)

    def or[B >: A](another: => Parser[B]): Parser[B] = self.or(theParser, another)

    def product[B >: A](another: Parser[B]): Parser[(A, B)] = self.product(theParser, another)

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
