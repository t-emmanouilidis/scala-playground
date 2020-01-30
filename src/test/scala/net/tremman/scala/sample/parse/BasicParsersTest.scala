package net.tremman.scala.sample.parse

import net.tremman.scala.sample.parse.Parsers.Success
import org.scalatest.FunSuite

class BasicParsersTest extends FunSuite {

  private val parsers = new BasicParsers()

  import parsers._

  test("should be able to parse a simple string") {
    assert(string("abra")("abra cadabra") === Success("abra", "abra".length))
  }

  test("should be able to parse using a basic regex") {
    assert(regex("\\d".r)("1aa") === Success("1", 1))
  }

}
