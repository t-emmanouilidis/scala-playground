package net.tremman.scala.playground.error

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = None

  def flatMap[B](f: A => Option[B]): Option[B] = None

  def getOrElse[B >: A](default: => B): B = default

  def orElse[B >: A](ob: => Option[B]): Option[B] = ob

  def filter(f: A => Boolean): Option[A] = None
}

case class Some[+A](get: A) extends Option[A] {

  override def getOrElse[B >: A](default: => B): B = get

  override def map[B](f: A => B): Option[B] = Some(f(get))

  override def flatMap[B](f: A => Option[B]): Option[B] = f(get)

  override def orElse[B >: A](ob: => Option[B]): Option[B] = this

  override def filter(f: A => Boolean): Option[A] = if (f(get)) Some(get) else None
}

case object None extends Option[Nothing]

object Option {

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h.flatMap(hh => sequence(t).map(hh :: _))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => SeqUtils.map2(f(h), traverse(t)(f))((b: B, theList: List[B]) => b :: theList)
  }

}
