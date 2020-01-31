package net.tremman.scala.sample.parse

import net.tremman.scala.sample.parse.BasicParsers.ParseState
import net.tremman.scala.sample.parse.Json.JsonObject
import net.tremman.scala.sample.parse.Parsers._
import org.scalatest.FunSuite

class JsonTest extends FunSuite {

  test("should be able to parse an empty object") {
    val parsers = new BasicParsers()
    val state = ParseState(ParseLocation("{}"))
    val expectedJsonObj = JsonObject(Map.empty)
    assert(Json.jsonParser(parsers)(state) == Success(expectedJsonObj, 2))
  }

}
