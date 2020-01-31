package net.tremman.scala.playground.parse

import net.tremman.scala.playground.parse.BasicParsers.ParseState
import net.tremman.scala.playground.parse.Parsers.{ParseLocation, Success}
import org.scalatest.FunSuite

class BasicParsersTest extends FunSuite {

  private val parsers = new BasicParsers()

  import parsers._

  test("should be able to parse a simple string") {
    val parseState = ParseState(ParseLocation("abra cadabra"))
    assert(string("abra")(parseState) === Success("abra", "abra".length))
  }

  test("should be able to parse using a basic regex") {
    val parseState = ParseState(ParseLocation("1aa"))
    assert(regex("\\d".r)(parseState) === Success("1", 1))
  }

}
