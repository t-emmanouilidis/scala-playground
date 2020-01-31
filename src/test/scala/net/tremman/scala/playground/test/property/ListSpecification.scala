package net.tremman.scala.playground.test.property

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}

object ListSpecification extends Properties("List") {

  val intList: Gen[List[Int]] = Gen.listOf(Gen.choose(0, 100))

  property("reverse") = forAll(intList)(ls => ls.reverse.reverse == ls) &&
    forAll(intList)(ls => ls.headOption == ls.reverse.lastOption) &&
    forAll(intList)(ls => if (ls.size > 1) ls.reverse != ls else ls.reverse == ls)

}
