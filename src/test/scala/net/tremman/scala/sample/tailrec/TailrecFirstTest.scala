package net.tremman.scala.sample.tailrec

import org.scalatest.FunSuite

class TailrecFirstTest extends FunSuite {

  test("TailrecFirst.fibonacci") {
    assert(TailrecFirst.fibonacci(0) === 0)
    assert(TailrecFirst.fibonacci(1) === 1)
    assert(TailrecFirst.fibonacci(2) === 1)
    assert(TailrecFirst.fibonacci(3) === 2)
    assert(TailrecFirst.fibonacci(4) === 3)
    assert(TailrecFirst.fibonacci(5) === 5)
    assert(TailrecFirst.fibonacci(6) === 8)
  }

  test("TailrecFirst.isSorted") {
    val orderedFunction = (a: Int, b: Int) => a < b

    assert(TailrecFirst.isSorted(Array(1), orderedFunction))
    assert(TailrecFirst.isSorted(Array(1, 2, 3, 4), orderedFunction))
    assert(!TailrecFirst.isSorted(Array(1, 2, 4, 3), orderedFunction))
  }

}
