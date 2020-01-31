package net.tremman.scala.playground.list

import org.scalatest.FunSuite

class ListTest extends FunSuite {

  test("drop") {
    assert(List.drop(List(1, 2, 3, 4), 3) === List(4))
  }

  test("tail") {
    assert(List.tail(List(1, 2)) === List(2))
  }

  test("dropWhile") {
    assert(List.dropWhile(List(1, 2, 3, 4))(a => a != 4) === List(4))
  }

  test("init") {
    assert(List.init(List(1, 2, 3, 4)) === List(1, 2, 3))
  }

  test("foldRight") {
    val l = List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
    println(l)
    assert(l === List(1, 2, 3))
  }

  test("length") {
    assert(List.length(List(1, 2, 3)) === 3)
  }

  test("sumWithFoldLeft") {
    assert(List.sumWithFoldLeft(List(1, 2, 3)) === 6)
  }

  test("productWithFoldLeft") {
    assert(List.productWithFoldLeft(List(1.0, 2.0, 3.0)) === 6.0)
  }

  test("lengthWithFoldLeft") {
    assert(List.lengthWithFoldLeft(List("1", "2", "3", "4")) === 4)
    assert(List.lengthWithFoldLeft(List(1, 2)) === 2)
  }

  test("reverseWithFoldLeft") {
    assert(List.reverseWithFoldLeft(Nil) === Nil)
    assert(List.reverseWithFoldLeft(List(1, 2, 3, 4)) === List(4, 3, 2, 1))
  }

  test("append") {
    assert(List.append(List(1, 2), List(3)) === List(1, 2, 3))
  }

  test("concatenate") {
    val theLists = List(
      List(1, 2, 3),
      List(4, 5, 6),
      List(7, 8, 9))

    assert(List.concatenate(theLists) === List(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  test("incrementInts") {
    assert(List.incrementInts(List(1, 2, 3, 4)) === List(2, 3, 4, 5))
  }

  test("map") {
    assert(List.map(List(1, 2, 3))((a: Int) => a + 1) === List(2, 3, 4))
  }

  test("filter") {
    assert(List.filter(List(1, 2, 3))((a: Int) => a % 2 == 0) === List(2))
    assert(List.filter(List("hello", "world", "hi", "all"))
    ((aString: String) => aString.length > 3) === List("hello", "world"))
  }

  test("flatMap") {
    assert(List.flatMap(List(1, 2, 3))(i => List(i, i)) === List(1, 1, 2, 2, 3, 3))
    assert(List.flatMap(List(1, 2))(i => List(i, i * 2)) === List(1, 2, 2, 4))
  }

  test("filterViaFlatMap") {
    assert(List.filterViaFlatMap(List(1, 2, 3))((a: Int) => a % 2 == 0) === List(2))
  }

  test("addIntegerLists") {
    assert(List.addIntegerLists(List(1, 2, 3), List(4, 5, 6)) === List(5, 7, 9))
    assert(List.addIntegerLists(List(1, 2, 3, 4), List(5, 6, 7)) === List(6, 8, 10, 4))
    assert(List.addIntegerLists(List(1, 2, 3), List(4, 5, 6, 7)) === List(5, 7, 9, 7))
  }

  test("hasSubsequence") {
    assert(List.hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
    assert(List.hasSubsequence(List(1, 2, 3, 4, 5), List(2, 3, 4)))
    assert(List.hasSubsequence(List(1, 2, 3, 4), List(4)))
    assert(!List.hasSubsequence(Nil, List(4)))
    assert(!List.hasSubsequence(List(1, 2, 3), Nil))
  }

  test("digits") {
    // Set
    def digits(n: Int): List[Int] = n match {
      case n if n == 0 => Nil
      case n => Cons(n % 10, digits(n / 10))
    }

    // Act
    val l: List[Int] = digits(135)

    // Assert
    assert(List.reverseWithFoldLeft(l) === List(1, 3, 5))
  }

}
