package net.tremman.scala.sample.parallel

import java.util.concurrent._

object Par {

  type Par[A] = ExecutorService => Future[A]

  case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = es => {
    val a = pa(es).get()
    val b = pb(es).get()
    UnitFuture(f(a, b))
  }

  def eval(es: ExecutorService)(r: => Unit): Unit = es.submit(new Callable[Unit] {
    override def call(): Unit = r
  })

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(() => a(es).get())

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](es: ExecutorService)(p: Par[A]): Future[A] = p(es)

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](ps: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] = ps.map(asyncF(a => if (f(a)) List(a) else List()))
    map(sequence(pars))(aList => aList.flatten)
  }

  def sequence[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)(_ :: _))

  def equal[A](es: ExecutorService)(pa: Par[A], pb: Par[A]): Boolean = {
    pa(es).get == pb(es).get
  }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(b => if (b) 0 else 1))(List(t, f))

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
    val key: Int = run(es)(n).get()
    UnitFuture(choices.apply(key)(es).get())
  }

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = chooser(key)(choices)

  def chooser[K, V](key: Par[K])(choiceF: K => Par[V]): Par[V] = es => {
    val k: K = run(es)(key).get()
    run(es)(choiceF(k))
  }

  def flatMap[A, B](p: Par[A])(choices: A => Par[B]): Par[B] = es => {
    val a: A = run(es)(p).get()
    run(es)(choices(a))
  }

  def flatMap2[A, B](p: Par[A])(choices: A => Par[B]): Par[B] = join(map(p)(choices))

  def join[A](p: Par[Par[A]]): Par[A] = es => {
    val pa: Par[A] = p(es).get()
    val a: A = pa(es).get()
    UnitFuture(a)
  }

  def join2[A](p: Par[Par[A]]): Par[A] = flatMap(p)((pa: Par[A]) => pa)


}
