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

  def foldMapV[A, B](ls: List[A], mb: Monoid[B])(f: A => B): B = {
    if (ls.isEmpty)
      mb.zero
    if (ls.size == 1)
      f(ls.head)
    else {
      val (left, right) = ls.splitAt(ls.size / 2)
      mb.op(foldMapV(left, mb)(f), foldMapV(right, mb)(f))
    }
  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(oneParser: Par[A], anotherParser: Par[A]): Par[A] = Par.map2(oneParser, anotherParser)(m.op)

    override def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A, B](v: List[A], m: Monoid[B])(f: A => B): Par[B] = foldMapV(v, par(m))(a => Par.lazyUnit(f(a)))

  // nice way of checking if a list is ordered
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

    // we start with (1, 1, true), (2, 2, true) etc. and we continue combining adjacent elements
    // checking that the right element of the first is smaller than the left element of the second
    foldMapV(ints, monoid)(i => Some(i, i, true)).forall(_._3)
  }

  def productMonoid[A, B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) = (ma.op(a1._1, a2._1), mb.op(a1._2, a2._2))

    override def zero: (A, B) = (ma.zero, mb.zero)
  }

  def mapMergeMonoid[K, V](mv: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    override def zero: Map[K, V] = Map[K, V]()

    // concat the key sets of each map together and then fold and apply the value monoid's
    // operation to each value pair
    override def op(a: Map[K, V], b: Map[K, V]): Map[K, V] = a.keySet.++(b.keySet)
      .foldLeft(zero)((acc: Map[K, V], k: K) =>
        acc.updated(k, mv.op(a.getOrElse(k, mv.zero), b.getOrElse(k, mv.zero))))
  }

  def functionMonoid[A, B](mb: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(f: A => B, g: A => B): A => B = a => mb.op(f(a), g(a))

    override def zero: A => B = _ => mb.zero
  }

  // Calls foldMapV with params:
  // 1st param: the list
  // 2nd param: the monoid which is able to take two maps and merge them
  // in case of conflict it adds their values which are integers
  // 3rd param: the function that transforms an element of the list to a Map
  def bag[A](as: List[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))((a: A) => Map[A, Int](a -> 1))

}
