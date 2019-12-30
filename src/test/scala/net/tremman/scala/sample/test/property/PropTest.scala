package net.tremman.scala.sample.test.property

import org.scalatest.FunSuite

class PropTest extends FunSuite {

  test("listOfN") {
    // Set
    val basicGenerator = Gen.unit(1)

    // Act
    val listGen = basicGenerator.listOfN(Gen.unit(4))

    // Assert
    assert(listGen.isInstanceOf[Gen[Int]])
  }

}
