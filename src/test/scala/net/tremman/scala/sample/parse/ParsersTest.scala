package net.tremman.scala.sample.parse

import org.scalatest.FunSuite

class ParsersTest extends FunSuite {

  test("char.basic") {
    val parsers: Parsers[ParseError, Parser[Char]] = null
    val c: Char = 'c'
    assert(parsers.run(parsers.char(c))(c.toString) === Right(c))
  }

  test("string.basic") {
    val parsers: Parsers[ParseError, Parser[String]] = null
    val str: String = "aString"
    assert(parsers.run(parsers.string(str))(str) === Right(str))
  }

}
