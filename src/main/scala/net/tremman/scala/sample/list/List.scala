package net.tremman.scala.sample.list

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](x: A, xs: List[A]) extends List[A]

object List {

  def reverseWithFoldLeft[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((z: List[A], a: A) => Cons(a, z))

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def append[A](theList: List[A], theOtherList: List[A]): List[A] = {
    foldRight(theList, theOtherList)((anElement: A, accumulator: List[A]) =>
      Cons(anElement, accumulator))
  }

  def concatenate[A](theLists: List[List[A]]): List[A] =
    foldLeft(theLists, Nil: List[A])((theTempList: List[A], aList: List[A]) =>
      append(theTempList, aList))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((anElement: A, tempList: List[B]) =>
      Cons(f(anElement), tempList))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((anElement: A, tempList: List[A]) =>
      if (f(anElement)) Cons(anElement, tempList) else tempList)

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)((anElement: A) => if (f(anElement)) Cons(anElement, Nil) else Nil)

  def zipWith[A](as: List[A], bs: List[A])(f: (A, A) => A): List[A] = {
    def loop(xs: List[A], ys: List[A]): List[A] = (xs, ys) match {
      case (Nil, Nil) => Nil
      case (Nil, Cons(_, _)) => ys
      case (Cons(_, _), Nil) => xs
      case (Cons(x, xss), Cons(y, yss)) => Cons(f(x, y), loop(xss, yss))
    }

    loop(as, bs)
  }

  def addIntegerLists(as: List[Int], bs: List[Int]): List[Int] = {
    zipWith(as, bs)(_ + _)
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def go(xs: List[A], ys: List[A]): Boolean = (xs, ys) match {
      // an eite i mia lista eite i alli exei eksantlithei kseroume oti den exoume match
      case (Nil, _) | (_, Nil) => false
      // an checkaroume to teleytaio element tou pattern,
      // eite exoume match eite proxwrame sto epomeno element tou text
      // gia na kanoume restart to search stin epomeni thesi
      case (Cons(x, xss), Cons(y, Nil)) => x == y || go(xss, sub)
      // ean kai oi 2 listes den exoun eksantlithei ean kanoun match ta 2 elements
      // tote proxwrame sta epomena elements twn listwn alliws proxwrame mono sto epomeno
      // element tou text
      case (Cons(x, xss), Cons(y, yss)) => if (x == y) go(xss, yss) else go(xss, sub)
    }

    go(sup, sub)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldLeft(as, Nil: List[B])((tempList: List[B], anElement: A) =>
      append(tempList, f(anElement)))

  def incrementInts(theIntegers: List[Int]): List[Int] =
    foldRight(theIntegers, Nil: List[Int])((anInt: Int, theTempList: List[Int]) =>
      Cons(anInt + 1, theTempList))

  def doublesToStrings(theDoubles: List[Double]): List[String] =
    foldRight(theDoubles, Nil: List[String])((aDouble: Double, theTempList: List[String]) =>
      Cons(aDouble.toString, theTempList))

  def productWithFoldLeft(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)

  def sumWithFoldLeft(as: List[Int]): Int = foldLeft(as, 0)(_ + _)

  def lengthWithFoldLeft(as: List[_]): Int = foldLeft(as, 0)((acc, _) => acc + 1)

  def length(as: List[_]): Int = as match {
    case Nil => 0
    case Cons(_, xs) => 1 + length(xs)
  }

  def sum(ints: List[Int]): Int = foldRight(ints, 0)(_ + _)

  def product(ds: List[Double]): Double = foldRight(ds, 1.0)(_ * _)

  def tail[A](aList: List[A]): List[A] = drop(aList, 1)

  def setHead[A](aList: List[A], a: A): List[A] = aList match {
    case Nil => Nil
    case Cons(x, xs) => Cons(a, xs)
  }

  @tailrec
  def drop[A](aList: List[A], n: Int): List[A] = aList match {
    case Nil => Nil
    case Cons(_, xs) => if (n == 1) xs else drop(xs, n - 1)
  }

  @tailrec
  def dropWhile[A](aList: List[A])(f: A => Boolean): List[A] = aList match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs)(f) else aList
  }

  def init[A](aList: List[A]): List[A] = aList match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

}
