package net.tremman.scala.playground.parallel

import java.util.concurrent.{ExecutorService, Executors}

import net.tremman.scala.playground.parallel.Par.Par
import org.scalatest.FunSuite

class ParTest extends FunSuite {

  def sum(seq: IndexedSeq[Int]): Par[Int] = {
    if (seq.length <= 1) {
      Par.unit(seq.headOption.getOrElse(0))
    } else {
      val (l, r) = seq.splitAt(seq.length / 2)
      // sum the first half and the second half in parallel
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }
  }

  test("sum.simple") {
    val es: ExecutorService = Executors.newFixedThreadPool(3)
    // based on the implementation of sum - above - we need one thread per
    // half vector in each iteration
    assert(Par.equal(es)(sum(Vector(1, 2, 3, 4, 5)), Par.unit(15)))
  }

  test("unit") {
    val es: ExecutorService = Executors.newFixedThreadPool(1)
    assert(Par.equal(es)(Par.map(Par.unit(1))(_ + 1), Par.unit(2)))
  }

}
