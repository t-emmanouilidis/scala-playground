package net.tremman.scala.playground.parse

import net.tremman.scala.playground.parse.BasicParsers.ParseState
import net.tremman.scala.playground.parse.Json.JsonObject
import net.tremman.scala.playground.parse.Parsers._
import org.scalatest.FunSuite

class JsonTest extends FunSuite {

  test("should be able to parse an empty object") {
    val parsers = new JsonParsers()
    val state = ParseState(ParseLocation("{}"))
    val expectedJsonObj = JsonObject(Map.empty)
    assert(Json.jsonParser(parsers)(state) == Success(expectedJsonObj, 2))
  }

}
