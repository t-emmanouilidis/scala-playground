package net.tremman.scala.sample.parse

import net.tremman.scala.sample.parse.BasicParsers.BasicParser
import net.tremman.scala.sample.parse.Parsers.ParseError.errorOccurredOn
import net.tremman.scala.sample.parse.Parsers._

import scala.language.{higherKinds, implicitConversions}
import scala.util.matching.Regex

object BasicParsers {
  type BasicParser[+A] = String => Result[A]
}

class BasicParsers extends Parsers[BasicParser] {
  override def run[A](p: BasicParser[A])(input: String): Result[A] = p(input)

  override def slice[A](p: BasicParser[A]): BasicParser[String] =
    (input: String) => p(input) match {
      case Success(_, n) => Success(input.slice(0, n), n)
      case f@Failure(_, _) => f
    }

  override def or[A](one: BasicParser[A], another: BasicParser[A]): BasicParser[A] = input =>
    one(input) match {
      case Failure(_, false) => another(input)
      case s => s
    }

  override implicit def string(theString: String): BasicParser[String] = (input: String) => {
    if (input.startsWith(theString))
      Success(theString, theString.length)
    else
      Failure(ParseError.errorOccurredOn(ParseLocation(input), "Expected: " + theString))
  }

  override def flatMap[A, B](aParser: BasicParser[A])(f: A => BasicParser[B]): BasicParser[B] = ???

  override def regex(theRegex: Regex): BasicParser[String] = (input: String) => {
    lazy val msg = theRegex + " did not match"
    theRegex.findPrefixOf(input) match {
      case Some(theMatch) => Success(theMatch, theMatch.length)
      case None => Failure(errorOccurredOn(ParseLocation(input), msg))
    }
  }

  override def errorLabel[A](errorLabel: String)(aParser: BasicParser[A]): BasicParser[A] =
    input => aParser(input).mapError(e => e.label(errorLabel))

  override def scope[A](scopeLabel: String)(aParser: BasicParser[A]): BasicParser[A] =
    input => aParser(input).mapError(e => e.push(ParseLocation(input), scopeLabel))

  override def attempt[A](aParser: BasicParser[A]): BasicParser[A] = input => aParser(input).uncommit
}
