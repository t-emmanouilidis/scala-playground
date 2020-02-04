package net.tremman.scala.playground.error

sealed trait Option[+A] {

  // primitive
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(get) => f(get)
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(get) => get
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case s@Some(_) => s
    case None => ob
  }

  // derived
  def map[B](f: A => B): Option[B] = flatMap(a => Some(f(a)))

  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

  def map2[B, C](another: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a <- this
      b <- another
    } yield f(a, b)
}

case class Some[+A](get: A) extends Option[A] {
}

case object None extends Option[Nothing]

object Option {

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def sequence[A](ls: List[Option[A]]): Option[List[A]] =
    ls.foldRight(emptyOptionList[A])((oa: Option[A], ola: Option[List[A]]) =>
      oa.map2(ola)((a: A, la: List[A]) => a :: la))

  def traverse[A, B](ls: List[A])(f: A => Option[B]): Option[List[B]] =
    ls.foldRight(emptyOptionList[B])((a: A, olb: Option[List[B]]) =>
      f(a).map2(olb)((b: B, lb: List[B]) => b :: lb))

  private def emptyOptionList[A]: Option[List[A]] = Some(List[A]())

}
