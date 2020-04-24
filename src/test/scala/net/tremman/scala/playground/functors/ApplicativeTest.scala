package net.tremman.scala.playground.functors

import org.scalatest.FunSuite

class ApplicativeTest extends FunSuite {

  test("functor laws should hold") {
    val listApplicative = Applicative.listApplicative

    val f = (i: Int) => i + 1
    val g = (i: Int) => i * 2

    assert(listApplicative.map(List(1))(identity) === List(1))
    assert(listApplicative.map(listApplicative.map(List(1))(f))(g) ===
      listApplicative.map(List(1))(f andThen g))
  }


}
