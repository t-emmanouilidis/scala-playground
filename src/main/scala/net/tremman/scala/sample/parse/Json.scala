package net.tremman.scala.sample.parse

import net.tremman.scala.sample.parse.BasicJsonParsers.{JsonParser, Parser}

import scala.util.matching.Regex

object BasicJsonParsers {
  type Parser[A] = String => A

  type JsonParser = Parser[Json]
}

class BasicJsonParsers extends Parsers[BasicParseError, JsonParser] {

  override def run(p: JsonParser)(input: String): Either[BasicParseError, A] = ???

  override def slice[A](p: Parser[A]): Parser[String] = ???

  override def or[A](one: Parser[A], another: Parser[A]): Parser[A] = ???

  override implicit def string(str: String): Parser[String] = ???

  override def flatMap[A, B](aParser: Parser[A])(f: A => Parser[B]): Parser[B] = ???

  override def regex(regex: Regex): Parser[String] = ???

}

trait Json {

}

object Json {

  case object JsonNull extends Json

  case class JsonBool(val value: Boolean) extends Json

  def jsonParser[Err, Parser[+_]](parsers: Parsers[Err, Parser]): Parser[Json] = {
    import parsers._
    val spaces = char(' ').zeroOrMoreOf().slice()
    Json.jsonParser(parsers)
  }
}


