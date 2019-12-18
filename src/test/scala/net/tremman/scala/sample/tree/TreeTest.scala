package net.tremman.scala.sample.tree

import org.scalatest.FunSuite

class TreeTest extends FunSuite {

  test("size") {
    assert(Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) === 5)
    assert(Tree.size(Leaf(1)) === 1)
  }

  test("maximum") {
    assert(Tree.maximum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) === 4)
  }

  test("depth") {
    assert(Tree.depth(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) === 3)
  }

  test("map") {
    assert(Tree.map(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(_ * 2) ===
      Branch(Branch(Leaf(2), Leaf(4)), Leaf(6)))
  }

}
