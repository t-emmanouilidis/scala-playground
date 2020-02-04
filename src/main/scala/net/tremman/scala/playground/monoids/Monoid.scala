package net.tremman.scala.playground.monoids

import net.tremman.scala.playground.parallel.Par
import net.tremman.scala.playground.parallel.Par.Par

trait Monoid[A] {

  // satisfies associativity
  def op(a1: A, a2: A): A

  // identity value
  def zero: A

}

object Monoid {

  val stringMonoid: Monoid[String] = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2

    override def zero: String = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    override def zero: List[A] = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)

    override def zero: Option[A] = Option.empty
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1.andThen(a2)

    override def zero: A => A = identity
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldMapV[A, B](v: List[A], m: Monoid[B])(f: A => B): B = {
    if (v.isEmpty)
      m.zero
    if (v.size == 1)
      f(v.head)
    else {
      val (left, right) = v.splitAt(v.size / 2)
      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }
  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(oneParser: Par[A], anotherParser: Par[A]): Par[A] = Par.map2(oneParser, anotherParser)(m.op)

    override def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A, B](v: List[A], m: Monoid[B])(f: A => B): Par[B] = foldMapV(v, par(m))(a => Par.lazyUnit(f(a)))

  def ordered(ints: List[Int]): Boolean = {

    val monoid = new Monoid[Option[(Int, Int, Boolean)]] {
      override def op(a1: Option[(Int, Int, Boolean)],
                      a2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] = (a1, a1) match {
        case (Some((x1, y1, p)), Some((x2, y2, q))) =>
          Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
        case (x, None) => x
        case (None, x) => x
      }

      override def zero: Option[(Int, Int, Boolean)] = None
    }

    foldMapV(ints, monoid)(i => Some(i, i, true)).map(_._3).getOrElse(true)
  }

}
