package net.tremman.scala.playground.monoids

import org.scalatest.FunSuite

class MonoidTest extends FunSuite {

  test("mapMergeMonoid") {
    val m1: Map[String, Map[String, Int]] = Map("o1" -> Map("i1" -> 1, "i2" -> 2))
    val m2: Map[String, Map[String, Int]] = Map("o1" -> Map("i2" -> 3))

    import Monoid._
    val M: Monoid[Map[String, Map[String, Int]]] = mapMergeMonoid(mapMergeMonoid(intAddition))
    val m3 = M.op(m1, m2)
    println(m3)

    assert(m3 === Map("o1" -> Map("i1" -> 1, "i2" -> 5)))
  }

  test("list length and sum") {
    import Monoid._
    val M: Monoid[(Int, Int)] = productMonoid(intAddition, intAddition)

    import Foldable._
    val (length, sum) = listFoldable.foldMap(List(1, 2, 3, 4))(i => (1, i))(M)

    assert(length === 4)
    assert(sum === 10)
  }

}
