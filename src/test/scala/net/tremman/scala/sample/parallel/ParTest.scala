package net.tremman.scala.sample.parallel

import java.util.concurrent.{ExecutorService, Executors}

import net.tremman.scala.sample.parallel.Par.Par
import org.scalatest.FunSuite

class ParTest extends FunSuite {

  def sum(seq: IndexedSeq[Int]): Par[Int] = {
    if (seq.length <= 1) {
      Par.unit(seq.headOption.getOrElse(0))
    } else {
      val (l, r) = seq.splitAt(seq.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }
  }

  test("sum.simple") {
    val es: ExecutorService = Executors.newFixedThreadPool(1)
    assert(Par.equal(es)(sum(Vector(1, 2, 3)), Par.unit(6)))
  }

  test("unit") {
    val es: ExecutorService = Executors.newFixedThreadPool(1)
    assert(Par.equal(es)(Par.map(Par.unit(1))(_ + 1), Par.unit(2)))
  }

}
