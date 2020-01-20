package net.tremman.scala.sample.parse

import org.scalatest.FunSuite

class ParsersTest extends FunSuite {

  test("char.basic") {
    val parsers: Parsers[ParseError, Parser[Char]] = ???
    val c: Char = 'c'
    assert(parsers.run(parsers.char(c))(c.toString) === Right(c))
  }

  test("string.basic") {
    val parsers: Parsers[ParseError, Parser[String]] = ???
    val str: String = "aString"
    assert(parsers.run(parsers.string(str))(str) === Right(str))
  }

  test("or.basic") {
    val parsers: Parsers[ParseError, Parser[String]] = ???
    import parsers._
    val theParser: Parser[String] = "abra" | "cadabra"
    assert(parsers.run(theParser)("abra") === Right("abra"))
    assert(parsers.run(theParser)("cadabra") === Right("cadabra"))
  }

  test("or parser should be commutative") {
    val parsers: Parsers[ParseError, Parser[String]] = ???
    import parsers._
    val oneParser: Parser[String] = "abra" | "cadabra"
    val otherParser: Parser[String] = "cadabra" | "abra"
    assert(parsers.run(oneParser)("abra") === parsers.run(otherParser)("abra"))
  }

  test("or parser should be associative") {
    val parsers: Parsers[ParseError, Parser[String]] = ???
    import parsers._
    val oneParser = "a".or("b").or("c")
    val otherParser = "b".or("c").or("a")
    assert(parsers.run(oneParser)("abc") === parsers.run(otherParser)("abc"))
  }

  test("list.basic") {
    val parsers: Parsers[ParseError, Parser[String]] = ???
    import parsers._
    assert(run(listOfN(3, "ab" | "cad"))("ababcad") === Right("ababcad"))
    assert(run(listOfN(3, "ab" | "cad"))("cadabab") === Right("cadabab"))
    assert(run(listOfN(3, "ab" | "cad"))("cadabab") === Right("ababab"))
  }

  test("basic int zero or more") {
    val parsers: Parsers[ParseError, Parser[Int]] = ???
    import parsers._
    assert(run(zeroOrMore("a"))("aaa") === Right(3))
    assert(run(zeroOrMore("aaa"))("aaa") === Right(1))
    assert(run(zeroOrMore("a"))("b123") === Right(0))
  }

  test("basic int one or more") {
    val parsers: Parsers[ParseError, Parser[Int]] = ???
    import parsers._
    assert(run(oneOrMore("a"))("aaa") === Right(3))
    assert(run(oneOrMore("aaa"))("aaa") === Right(1))
    assert(run(oneOrMore("a"))("b123") === Left(BasicParseError("Expected one or more 'a'")))
  }

  test("basic followed by") {
    val parsers: Parsers[ParseError, Parser[Int]] = ???
    import parsers._
    // the first parser is converted implicitly to a ParserOps instance which has the method followedBy
    assert(run(zeroOrMore("a").followedBy(oneOrMore("b")))("aaab") === Right((3, 1)))
    assert(run(zeroOrMore("a").followedBy(oneOrMore("b")))("bbb") === Right((0, 3)))
    assert(run(zeroOrMore("a").followedBy(oneOrMore("b")))("aaaab") === Right((4, 1)))
  }

}
