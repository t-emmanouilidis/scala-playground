package net.tremman.scala.sample.parse

import net.tremman.scala.sample.parse.BasicParsers.{BasicParser, ParseState}
import net.tremman.scala.sample.parse.Parsers.ParseError.errorOccurredOn
import net.tremman.scala.sample.parse.Parsers._

import scala.language.{higherKinds, implicitConversions}
import scala.util.matching.Regex

object BasicParsers {
  type BasicParser[+A] = ParseState => Result[A]

  case class ParseState(location: ParseLocation) {
    def slice(n: Int): String = location.getUpTo(n)

    def matchesString(aString: String): Boolean = location.inputAtOffsetStartsWith(aString)

    def matchesRegex(aRegex: Regex): Option[String] = location.inputAtOffsetMatchesRegex(aRegex)

    def advanceBy(charsNum: Int): ParseState = copy(location = location.advanceBy(charsNum))
  }

}

class BasicParsers extends Parsers[BasicParser] {
  override def run[A](p: BasicParser[A])(input: String): Result[A] = {
    val state = ParseState(ParseLocation(input))
    p(state)
  }

  override def slice[A](p: BasicParser[A]): BasicParser[String] =
    state => p(state) match {
      case Success(_, n) => Success(state.slice(n), n)
      case f@Failure(_, _) => f
    }

  override def or[A](one: BasicParser[A], another: => BasicParser[A]): BasicParser[A] = input =>
    one(input) match {
      case Failure(_, false) => another(input)
      case s => s
    }

  override implicit def string(theString: String): BasicParser[String] = (state: ParseState) => {
    if (state.matchesString(theString))
      Success(theString, theString.length)
    else
      Failure(ParseError.errorOccurredOn(state.location, "Expected: " + theString))
  }

  override def flatMap[A, B](aParser: BasicParser[A])(f: A => BasicParser[B]): BasicParser[B] = state => {
    aParser(state) match {
      case Success(a, charsConsumed) =>
        f(a)(state.advanceBy(charsConsumed)).addCommit(charsConsumed > 0)
      case e@Failure(_, _) => e
    }
  }

  override def regex(theRegex: Regex): BasicParser[String] = (state: ParseState) => {
    assert(theRegex != null, "theRegex can't be null")

    lazy val msg = theRegex + " did not match"
    state.matchesRegex(theRegex) match {
      case Some(theMatch) => Success(theMatch, theMatch.length)
      case None => Failure(errorOccurredOn(state.location, msg))
    }
  }

  override def errorLabel[A](errorLabel: String)(aParser: BasicParser[A]): BasicParser[A] =
    input => aParser(input).mapError(e => e.label(errorLabel))

  override def scope[A](scopeLabel: String)(aParser: BasicParser[A]): BasicParser[A] =
    state => aParser(state).mapError(e => e.push(state.location, scopeLabel))

  override def attempt[A](aParser: BasicParser[A]): BasicParser[A] = input => aParser(input).uncommit
}
