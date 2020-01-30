package net.tremman.scala.sample.parse

import net.tremman.scala.sample.parse.Parsers._
import net.tremman.scala.sample.test.property.{Gen, Prop, SGen}

import scala.language.{higherKinds, implicitConversions}
import scala.util.matching.Regex

// trait Parsers here is a second-order type,
// abstracts over a type constructor which abstracts over a first-order type
// abstraction over a first-order type
// Parser[+_] is a type constructor here
// (a type with a type constructor is called a higher kinded type)
trait Parsers[Parser[+_]] {
  self =>

  def run[A](p: Parser[A])(input: String): Result[A]

  // primitives
  def slice[A](p: Parser[A]): Parser[String]

  def or[A](one: Parser[A], another: Parser[A]): Parser[A]

  implicit def string(str: String): Parser[String]

  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  def flatMap[A, B](aParser: Parser[A])(f: A => Parser[B]): Parser[B]

  def regex(regex: Regex): Parser[String]

  def errorLabel[A](errorLabel: String)(aParser: Parser[A]): Parser[A]

  def scope[A](scopeLabel: String)(aParser: Parser[A]): Parser[A]

  def attempt[A](aParser: Parser[A]): Parser[A]

  // non primitives
  def digit: Parser[String] = regex("\\d".r)

  def letter: Parser[String] = regex("[a-zA-Z]".r)

  def whitespace(str: String): Parser[String] = regex("\\s".r)

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

  def zeroOrMoreOf[A](aParser: Parser[A]): Parser[List[A]] =
    map2(aParser, zeroOrMoreOf(aParser))(_ :: _) or succeed(List())

  def oneOrMoreOf[A](aParser: Parser[A]): Parser[List[A]] =
    map2(aParser, zeroOrMoreOf(aParser))(_ :: _)

  // this is the implicit function used to convert a Parser instance to a ParserOps instance
  implicit def operators[A](theParser: Parser[A]): ParserOps[A] = ParserOps(theParser)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def errorLocation(e: ParseError): ParseLocation = e.stack.head._1

  def errorMessage(e: ParseError): String = e.stack.head._2

  case class ParserOps[A](theParser: Parser[A]) {

    // primitive
    def slice(): Parser[String] = self.slice(theParser)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(theParser)(f)

    def or[B >: A](another: => Parser[B]): Parser[B] = self.or(theParser, another)

    // non primitive
    def map[B](f: A => B): Parser[B] = self.map(theParser)(f)

    def map2[B, C](another: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(theParser, another)(f)

    def zeroOrMoreOf(): Parser[List[A]] = self.zeroOrMoreOf(theParser)

    def oneOrMoreOf(): Parser[List[A]] = self.oneOrMoreOf(theParser)

    def |[B >: A](another: Parser[B]): Parser[B] = self.or(theParser, another)

    def product[B >: A](another: Parser[B]): Parser[(A, B)] = self.product(theParser, another)

    def **[B >: A](another: Parser[B]): Parser[(A, B)] = product(another)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(str => run(p1)(str) == run(p2)(str))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)

    def succeedLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(str => run(succeed(str))("1") == Success(str, str.length))

    def labelLaw[A](p: Parser[A])(in: SGen[String]): Prop =
      Prop.forAll(in ** Gen.string) {
        case (input, msg) =>
          run(errorLabel(msg)(p))(input) match {
            case Failure(e, _) => errorMessage(e) == msg
            case _ => true
          }
      }
  }

}

object Parsers {

  trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, committed) => Failure(f(e), committed)
      case _ => this
    }

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure.uncommitted(e)
      case _ => this
    }
  }

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]

  case class Failure private (get: ParseError, committed: Boolean = true) extends Result[Nothing]

  object Failure {
    def committed(error: ParseError): Failure = Failure(error)

    def uncommitted(error: ParseError): Failure = Failure(error, committed = false)
  }

  case class ParseLocation(input: String, offset: Int = 0) {
    lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val column: Int = input.slice(0, offset + 1).lastIndexOf('\n') match {
      case -1 => offset + 1
      case lineStart => offset - lineStart
    }
  }

  case class ParseError(stack: List[(ParseLocation, String)]) {
    def push(theLocation: ParseLocation, theMessage: String): ParseError =
      copy(stack = (theLocation, theMessage) :: stack)

    def label(theLabel: String): ParseError = copy(stack = latestLocation.map((_, theLabel)).toList)

    def latestLocation: Option[ParseLocation] = latestScope.map(_._1)

    def latestScope: Option[(ParseLocation, String)] = stack.lastOption
  }

  object ParseError {
    def errorOccurredOn(theLocation: ParseLocation, withMessage: String): ParseError =
      ParseError(List((theLocation, withMessage)))

  }

}
