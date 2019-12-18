package net.tremman.scala.sample.stream

import net.tremman.scala.sample.stream.Stream._
import org.scalatest.FunSuite

class StreamTest extends FunSuite {

  test("drop") {
    assert(Stream(1, 2, 3, 4, 5).drop(2).toList === List(3, 4, 5))
  }

  test("takeWhile") {
    assert(Stream(1, 2, 3, 4, 5).takeWhile(_ < 4).toList === List(1, 2, 3))
  }

  test("takeWhileWithFoldRight") {
    assert(Stream(1, 2, 3, 4, 5).takeWhileWithFoldRight(_ < 4).toList === List(1, 2, 3))
  }

  test("headOptionWithFoldRight") {
    assert(Stream().headOptionWithFoldRight === None)
    assert(Stream(1, 2, 3, 4, 5).headOptionWithFoldRight === Some(1))
    assert(Stream("test").headOptionWithFoldRight === Some("test"))
  }

  test("map") {
    assert(Stream(1, 2, 3).map(_ * 2).toList === List(2, 4, 6))
  }

  test("filter") {
    assert(Stream(1, 2, 3, 4, 5).filter(a => a != 2 && a != 4).toList === List(1, 3, 5))
  }

  test("append") {
    assert(Stream(1, 2, 3).append(Stream(4, 5)).toList === List(1, 2, 3, 4, 5))
  }

  test("forAll") {
    assert(Stream(1, 2, 3, 4, 5).forAll(_ < 6))
    assert(!Stream(7, 1, 2, 3, 4, 5).forAll(_ < 6))
  }

  test("constant") {
    val ones = Stream.constant(1)
    assert(ones.take(5).toList === List(1, 1, 1, 1, 1))

    val hellos = Stream.constant("Hello")
    assert(hellos.take(2).toList === List("Hello", "Hello"))
  }

  test("from") {
    assert(Stream.from(5).take(3).toList === List(5, 6, 7))
    assert(Stream.from(1).take(1).toList === List(1))
  }

  test("fibs") {
    assert(fibs().take(7).toList === List(0, 1, 1, 2, 3, 5, 8))
  }

  test("fromViaUnfold") {
    assert(fromViaUnfold(5).take(3).toList === List(5, 6, 7))
  }

  test("constantViaUnfold") {
    assert(constantViaUnfold("test").take(2).toList === List("test", "test"))
  }

  test("mapViaUnfold") {
    assert(mapViaUnfold(onesViaUnfold())(_ + 2).take(2).toList === List(3, 3))
  }

  test("takeViaUnfold") {
    assert(takeViaUnfold(onesViaUnfold(), 2).toList === List(1, 1))
  }

  test("takeWhileViaUnfold") {
    assert(takeWhileViaUnfold(Stream(1, 2, 3))(_ < 3).toList === List(1, 2))
  }

}
