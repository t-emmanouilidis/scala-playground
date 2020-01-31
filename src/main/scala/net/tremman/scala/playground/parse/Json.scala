package net.tremman.scala.playground.parse

import net.tremman.scala.playground.parse.BasicParsers.{BasicParser, ParseState}

import scala.language.{higherKinds, implicitConversions}

trait Json

object Json {

  case object JsonNull extends Json

  case class JsonBool(value: Boolean) extends Json

  case class JsonNumber(value: Double) extends Json

  case class JsonString(value: String) extends Json

  case class JsonArray(value: Array[Json]) extends Json

  case class JsonObject(value: Map[String, Json]) extends Json

  type JsonParser = BasicParser[Json]

  def jsonParser(P: JsonParsers): JsonParser = (parseState: ParseState) => null

}

class JsonParsers extends BasicParsers {

}
