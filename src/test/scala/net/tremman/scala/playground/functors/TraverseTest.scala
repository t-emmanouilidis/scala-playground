package net.tremman.scala.playground.functors

import org.scalatest.FunSuite

class TraverseTest extends FunSuite {

  test("should be able to zip with index using state traversal") {
    val listTraversable = Traverse.listTraversable

    assert(listTraversable._zipWithIndex(List('a', 'b')) === List(('a', 0), ('b', 1)))
  }

  test("should be able to create a list from a traversable") {
    val treeTraversable = Traverse.treeTraversable
    val theTree = Tree(2, List(Tree(1, Nil), Tree(3, Nil)))

    assert(treeTraversable._toList(theTree) === List(2, 1, 3))
  }

  test("reverse should obey law") {
    val T = Traverse.listTraversable
    val x = List(1, 2)
    val y = List(3, 4)

    assert(T.toList(T.reverse(x)) ++ T.toList(T.reverse(y)) === T.reverse(T.toList(y) ++ T.toList(x)))
  }

}
