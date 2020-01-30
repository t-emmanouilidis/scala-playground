package net.tremman.scala.sample.parse

import scala.language.{higherKinds, implicitConversions}

trait Json

object Json {

  case object JsonNull extends Json

  case class JsonBool(value: Boolean) extends Json

  case class JsonNumber(value: Double) extends Json

  case class JsonString(value: String) extends Json

  case class JsonArray(value: Array[Json]) extends Json

  case class JsonObject(value: Map[String, Json]) extends Json

  def jsonParser[ParseError, Parser[+_]](P: Parsers[Parser]): Parser[Json] = ???

}
