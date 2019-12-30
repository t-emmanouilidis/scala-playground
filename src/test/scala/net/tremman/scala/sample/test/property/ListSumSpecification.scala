package net.tremman.scala.sample.test.property

import org.scalacheck.{Gen, Prop, Properties}

object ListSumSpecification extends Properties("list.sum") {

  private val intList: Gen[List[Int]] = Gen.listOf(Gen.choose(0, 100))
  private val constantIntList: Gen[List[Int]] = Gen.listOf(Gen.const(1))

  property("sum of empty list is zero") =
    Prop.forAll(intList)(ls => if (ls.isEmpty) ls.sum == 0 else true)

  property("sum of list with same value elements") =
    Prop.forAll(constantIntList)(ls => ls.sum == ls.size * 1)

  property("sum of reverse list is equal to normal") =
    Prop.forAll(intList)(ls => ls.sum == ls.reverse.sum)

}
