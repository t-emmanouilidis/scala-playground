package net.tremman.scala.sample.parse

import org.scalatest.FunSuite

class ParsersTest extends FunSuite {

  def stringParsers(): Parsers[ParseError, Parser] = {
    val parsers: Parsers[ParseError, Parser] = ???
    parsers
  }

  def intParsers(): Parsers[ParseError, Parser] = {
    val parsers: Parsers[ParseError, Parser] = ???
    parsers
  }

  test("char.basic") {
    val parsers: Parsers[ParseError, Parser] = ???
    val c: Char = 'c'
    assert(parsers.run(parsers.char(c))(c.toString) === Right(c))
  }

  test("string.basic") {
    val parsers = stringParsers()
    val str: String = "aString"
    assert(parsers.run(parsers.string(str))(str) === Right(str))
  }

  test("or.basic") {
    val parsers = stringParsers()
    import parsers._
    val theParser: Parser[String] = "abra" | "cadabra"
    assert(parsers.run(theParser)("abra") === Right("abra"))
    assert(parsers.run(theParser)("cadabra") === Right("cadabra"))
  }

  test("or parser should be commutative") {
    val parsers = stringParsers()
    import parsers._
    val oneParser: Parser[String] = "abra" | "cadabra"
    val otherParser: Parser[String] = "cadabra" | "abra"
    assert(parsers.run(oneParser)("abra") === parsers.run(otherParser)("abra"))
  }

  test("or parser should be associative") {
    val parsers = stringParsers()
    import parsers._
    val oneParser = "a".or("b").or("c")
    val otherParser = "b".or("c").or("a")
    assert(parsers.run(oneParser)("abc") === parsers.run(otherParser)("abc"))
  }

  test("list.basic") {
    val parsers = stringParsers()
    import parsers._
    assert(parsers.run(listOfN(3, "ab" | "cad"))("ababcad") === Right("ababcad"))
    assert(parsers.run(listOfN(3, "ab" | "cad"))("cadabab") === Right("cadabab"))
    assert(parsers.run(listOfN(3, "ab" | "cad"))("cadabab") === Right("ababab"))
  }

  test("basic int zero or more") {
    val parsers = intParsers()
    import parsers._
    assert(parsers.run(zeroOrMoreOf("a"))("aaa") === Right(3))
    assert(parsers.run(zeroOrMoreOf("aaa"))("aaa") === Right(1))
    assert(parsers.run(zeroOrMoreOf("a"))("b123") === Right(0))
  }

  test("basic int one or more") {
    val parsers = intParsers()
    import parsers._
    assert(parsers.run(oneOrMoreOf("a"))("aaa") === Right(3))
    assert(parsers.run(oneOrMoreOf("aaa"))("aaa") === Right(1))
    assert(parsers.run(oneOrMoreOf("a"))("b123") === Left(BasicParseError("Expected one or more 'a'")))
  }

  test("basic product") {
    val parsers = intParsers()
    import parsers._
    // the first parser is converted implicitly to a ParserOps instance which has the method followedBy
    assert(parsers.run(zeroOrMoreOf("a").product(oneOrMoreOf("b")))("aaab") === Right((3, 1)))
    assert(parsers.run(zeroOrMoreOf("a").product(oneOrMoreOf("b")))("bbb") === Right((0, 3)))
    assert(parsers.run(zeroOrMoreOf("a").product(oneOrMoreOf("b")))("aaaab") === Right((4, 1)))
  }

  test("basic map") {
    val parsers = intParsers()
    import parsers._
    val numA: Parser[Int] = char('a').zeroOrMoreOf().map(_.size)
    assert(parsers.run(numA)("aaa") === Right(3))
    assert(parsers.run(numA)("b") === Right(0))
  }

  test("basic slice") {
    val parsers = stringParsers()
    import parsers._
    assert(parsers.run(slice((char('a') | char('b')).zeroOrMoreOf()))("aaba") === Right("aaba"))
  }

  test("basic context sensitivity") {
    val parsers = stringParsers()
    import parsers._
    val parser = digit.flatMap(n => listOfN(Integer.parseInt(n), letter)).slice()
    assert(parsers.run(parser)("4aaaa") === Right("4aaaa"))
  }

}

