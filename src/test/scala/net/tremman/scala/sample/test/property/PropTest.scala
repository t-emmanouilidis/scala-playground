package net.tremman.scala.sample.test.property

import java.util.concurrent.Executors

import net.tremman.scala.sample.parallel.Par
import net.tremman.scala.sample.parallel.Par.Par
import org.scalatest.FunSuite

import scala.annotation.tailrec

class PropTest extends FunSuite {

  test("listOfN") {
    // Set
    val basicGenerator = Gen.unit(1)

    // Act
    val listGen = basicGenerator.listOfN(Gen.unit(4))

    // Assert
    assert(listGen.isInstanceOf[Gen[Int]])
  }

  test("listOf.first") {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = Prop.forAll(Gen.listOf1(smallInt))(ls => {
      val max = ls.max
      !ls.exists(_ > max)
    })
    Prop.run(maxProp)
  }

  test("listOf.sorted") {
    val smallInt = Gen.choose(-10, 10)
    val sortedProp = Prop.forAll(Gen.listOf1(smallInt))(ls => {
      @tailrec
      def isSorted(ls: List[Int]): Boolean = ls match {
        case _ :: Nil => true
        case x :: xs => x < xs.head && isSorted(xs)
        case _ => true
      }

      isSorted(ls)
    })
    Prop.run(sortedProp)
  }

  test("par.map.unit") {
    val es = Executors.newCachedThreadPool

    def equal[A](one: Par[A], other: Par[A]): Par[Boolean] = Par.map2(one, other)(_ == _)

    val mapProp = Prop.check {
      equal(
        Par.map(Par.unit(1))(_ + 1),
        Par.unit(2)
      )(es).get()
    }
    Prop.run(mapProp)
  }



}
