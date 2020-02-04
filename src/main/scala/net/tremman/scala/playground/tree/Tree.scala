package net.tremman.scala.playground.tree

import scala.annotation.tailrec

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def map[A, B](aTree: Tree[A])(f: A => B): Tree[B] = {
    def go(aTree: Tree[A]): Tree[B] = aTree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(go(left), go(right))
    }

    go(aTree)
  }

  def depth[A](aTree: Tree[A]): Int = {
    @tailrec
    def go(theCurrentNode: Tree[A], currentDepth: Int): Int = theCurrentNode match {
      case Leaf(_) => currentDepth + 1
      case Branch(left, right) => if (left != null) go(left, currentDepth + 1)
      else if (right != null) go(right, currentDepth + 1) else 1
    }

    go(aTree, 0)
  }

  def size[A](aTree: Tree[A]): Int = aTree match {
    case Leaf(_) => 1
    case Branch(left: Tree[A], right: Tree[A]) => 1 + size(left) + size(right)
  }

  def maximum(aTree: Tree[Int]): Int = {
    def go(theCurrentNode: Tree[Int], theCurrentMax: Int): Int = theCurrentNode match {
      case Leaf(value) => value max theCurrentMax
      case Branch(left, right) => go(left, theCurrentMax) max go(right, theCurrentMax)
    }

    go(aTree, Integer.MIN_VALUE)
  }

}
