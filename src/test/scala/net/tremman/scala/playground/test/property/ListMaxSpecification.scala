package net.tremman.scala.playground.test.property

import org.scalacheck.{Gen, Prop, Properties}

object ListMaxSpecification extends Properties("list.max") {

  private val intList: Gen[List[Int]] = Gen.nonEmptyListOf(Gen.choose(0, 100))

  property("max should be the same for reverse list") =
    Prop.forAll(intList)(ls => ls.max == ls.reverse.max)

}
