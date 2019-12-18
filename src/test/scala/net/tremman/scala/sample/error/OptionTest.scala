package net.tremman.scala.sample.error

import org.scalatest.FunSuite

import scala.util.{Success, Try}

class OptionTest extends FunSuite {

  private val traverseFunc = (str: String) => Try {
    str.toInt
  } match {
    case Success(value) => Some(value)
    case _ => None
  }

  test("shouldBeAbleToTraverseValidOptions") {
    // Act
    val result = Option.traverse(List("1", "2", "3"))(traverseFunc)

    // Assert
    assert(result === Some(List(1, 2, 3)))
  }

  test("shouldNotBeAbleToTraverseInvalidOptions") {
    // Act
    val result = Option.traverse(List("1", "2", "test"))(traverseFunc)

    // Assert
    assert(result === None)
  }

  test("shouldBeAbleToSequenceValidOptions") {
    // Set
    val options = List(Some(1), Some(2), Some(3))

    // Act
    val result = Option.sequence(options)

    // Assert
    assert(result === Some(List(1, 2, 3)))
  }

  test("shouldNotBeAbleToSequenceInvalidOptions") {
    // Set
    val options = List(Some(1), Some(2), None)

    // Act
    val result = Option.sequence(options)

    // Assert
    assert(result === None)
  }

}
