package net.tremman.scala.sample.tailrec

import scala.annotation.tailrec

object TailrecFirst {
  def isSorted[A](array: Array[A], ordered: (A, A) => Boolean): Boolean = {
    assert(array.length > 0)

    if (array.length == 1) true
    else {
      @tailrec
      def loop(idx: Int): Boolean = {
        if (idx == array.length) true
        else (ordered(array(idx - 1), array(idx)) && loop(idx + 1))
      }

      loop(1)
    }
  }

  def main(args: Array[String]): Unit = {
    fibonacci(Integer.parseInt(args(0)))
  }

  def fibonacci(n: Int): Int = {
    def go(n: Int): Int = {
      if (n == 0) 0
      else if (n == 1) 1
      else go(n - 2) + go(n - 1)
    }

    go(n)
  }

}
