package net.tremman.scala.playground.state

import scala.annotation.tailrec

// S is the type of the status
// run is a transition from the current state to the new one
// together with the outcome of the state transition
case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    @tailrec
    def go(s: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) =
      actions match {
        case Nil => (acc.reverse, s)
        case h :: t => h.run(s) match {
          case (a, s2) => go(s2, t, a :: acc)
        }
      }

    State(s => go(s, sas, List()))
  }

  def sequenceViaFoldLeft[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)((a, ls) => a :: ls))

  def get[S]: State[S, S] = State(s => (s, s))

}
