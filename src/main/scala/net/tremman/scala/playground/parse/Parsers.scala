package net.tremman.scala.playground.parse

import net.tremman.scala.playground.parse.Parsers._
import net.tremman.scala.playground.test.property.{Gen, Prop, SGen}

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

  def or[A](one: Parser[A], another: => Parser[A]): Parser[A]

  implicit def string(str: String): Parser[String]

  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  def flatMap[A, B](aParser: Parser[A])(f: A => Parser[B]): Parser[B]

  def regex(regex: Regex): Parser[String]

  def errorLabel[A](errorLabel: String)(aParser: Parser[A]): Parser[A]

  def scope[A](scopeLabel: String)(aParser: Parser[A]): Parser[A]

  def attempt[A](aParser: Parser[A]): Parser[A]

  // derived

  // a parser that identifies a single digit
  def digit: Parser[String] = regex("\\d".r)

  // a parser that identifies a single letter
  def letter: Parser[String] = regex("[a-zA-Z]".r)

  // a parser that identifies a single whitespace
  def whitespace(str: String): Parser[String] = regex("\\s".r)

  implicit def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  // a complex parser that identifies the first string or the second string
  // if the first string is identified the parser never checks the second string
  def orString(str1: String, str2: String): Parser[String] = or(string(str1), string(str2))

  // a complex parser that identifies n occurrences of the given parser
  def listOfN[A](n: Int, aParser: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(aParser, listOfN(n - 1, aParser))(_ :: _)

  // flatMap expects a function that takes A and returns Parser[B]
  // the function resulted by composing f with succeed() is such a function
  def map[A, B](theParser: Parser[A])(f: A => B): Parser[B] =
    theParser.flatMap(f andThen succeed)

  // applies parser A, then parser B and returns a parser for their results' product
  def product[A, B](one: Parser[A], another: Parser[B]): Parser[(A, B)] = map2(one, another)((_, _))

  // applies parser A, then parser B and then the function f to their results
  // uses a for comprehension instead the following:
  // flatMap(one)(a => map(another)(b => f(a, b)))
  def map2[A, B, C](one: Parser[A], another: Parser[B])(f: (A, B) => C): Parser[C] =
    for {
      a <- one
      b <- another
    } yield f(a, b)

  // a complex parser that identifies zero or more of the given parser
  def zeroOrMoreOf[A](aParser: Parser[A]): Parser[List[A]] =
    map2(aParser, zeroOrMoreOf(aParser))(_ :: _) or succeed(List())

  // a complex parser that identifies one or more of the given parser
  def oneOrMoreOf[A](aParser: Parser[A]): Parser[List[A]] =
    map2(aParser, zeroOrMoreOf(aParser))(_ :: _)

  // this is the implicit function used to convert a Parser instance to a ParserOps instance
  implicit def operators[A](theParser: Parser[A]): ParserOps[A] = ParserOps(theParser)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  // basic implementation of returning the location of the given parse error
  def errorLocation(e: ParseError): ParseLocation = e.stack.head._1

  // basic implementation of returning the message of a parse error
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

    def addCommit(shouldCommit: Boolean): Result[A] = this match {
      case Failure(e, committed) => Failure(e, committed || shouldCommit)
      case _ => this
    }

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure.uncommitted(e)
      case _ => this
    }

    def advanceAlso(extraCharsNum: Int): Result[A] = this match {
      case Success(a, charsConsumed) => Success(a, charsConsumed + extraCharsNum)
      case _ => this
    }
  }

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]

  // if at least one character is consumed before the failure occurs, then
  // by default we commit to this parser
  case class Failure private(get: ParseError, committed: Boolean = true) extends Result[Nothing]

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

    def getUpTo(n: Int): String = input.substring(offset, offset + n)

    def inputAtOffsetStartsWith(aString: String): Boolean = {
      assert(aString != null, "aString can't be null")

      val theSlice = input.slice(offset, input.length)
      theSlice.startsWith(aString)
    }

    def inputAtOffsetMatchesRegex(aRegex: Regex): Option[String] = {
      assert(aRegex != null, "aRegex can't be null")

      val theSlice = input.slice(offset, input.length)
      aRegex.findPrefixOf(theSlice)
    }

    def advanceBy(n: Int): ParseLocation = copy(offset = offset + n)
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
