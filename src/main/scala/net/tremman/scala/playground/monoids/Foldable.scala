package net.tremman.scala.playground.monoids

import net.tremman.scala.playground.stream
import net.tremman.scala.playground.tree.{Branch, Leaf, Tree}

import scala.language.higherKinds

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

  def toList[A](fa: F[A]): List[A] = foldLeft(fa)(List[A]())((ls: List[A], a: A) => a :: ls)
}

object Foldable {
  val listFoldable: Foldable[List] = new Foldable[List] {

    import Monoid._

    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = foldMapV(as, mb)(f)

    override def toList[A](fa: List[A]): List[A] = fa
  }

  val indexedSeqFoldable: Foldable[IndexedSeq] = new Foldable[IndexedSeq] {

    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
  }

  val streamFoldable: Foldable[stream.Stream] = new Foldable[stream.Stream] {
    override def foldRight[A, B](as: stream.Stream[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    override def foldLeft[A, B](as: stream.Stream[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    override def foldMap[A, B](as: stream.Stream[A])(f: A => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
  }

  val treeFoldable: Foldable[Tree] = new Foldable[Tree] {
    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
      case Leaf(value) => f(value, z)
      case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f)
    }

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
      case Leaf(value) => f(z, value)
      case Branch(left, right) => foldLeft(right)(foldLeft(left)(z)(f))(f)
    }

    override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
      case Leaf(value) => f(value)
      case Branch(left, right) => mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
    }
  }

  val optionFoldable: Foldable[Option] = new Foldable[Option] {
    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
      case None => z
      case Some(value) => f(value, z)
    }

    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
      case None => z
      case Some(value) => f(z, value)
    }

    override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
      case None => mb.zero
      case Some(value) => mb.op(mb.zero, f(value))
    }
  }

}


