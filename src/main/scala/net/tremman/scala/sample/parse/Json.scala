package net.tremman.scala.sample.parse

import scala.util.matching.Regex

trait Json {

}

class JsonParsers extends Parsers[BasicParseError, Parser[String]] {

  override def run[A](p: Parser[A])(input: String): Either[BasicParseError, A] = ???

  override def slice[A](p: Parser[A]): Parser[String] = ???

  override def or[A](one: Parser[A], another: Parser[A]): Parser[A] = ???

  override implicit def string(str: String): Parser[String] = ???

  override def flatMap[A, B](aParser: Parser[A])(f: A => Parser[B]): Parser[B] = ???

  override def regex(regex: Regex): Parser[String] = ???

}

object Json {

  case object JsonNull extends Json

  case class JsonBool(val value: Boolean) extends Json

  def jsonParser[Err, Parser[+_]](parsers: Parsers[Err, Parser]): Parser[Json] = {
    import parsers._
    val spaces = char(' ').zeroOrMoreOf().slice()
    null
  }
}


