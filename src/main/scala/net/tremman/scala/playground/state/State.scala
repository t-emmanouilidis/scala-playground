package net.tremman.scala.playground.state

import java.util.concurrent.atomic.AtomicInteger

import scala.annotation.tailrec

object Count {
  private val value = new AtomicInteger(0)

  def increment: Int = {
    value.getAndIncrement()
  }
}

// S is the type of the status
// run is a transition from the current state to the new one
// together with the outcome of the state transition
case class State[S, +A](run: S => (A, S), name: String = "State-" + Count.increment) {

  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    val newState = f(a)
    val (b, s2) = newState.run(s1)
    (b, s2)
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

  // returns a new State whose run method takes an object s and returns a tuple (s, s)
  // i.e. returns the current state
  def get[S]: State[S, S] = State(run = s => (s, s))

  // returns a new State whose run method take () and returns the tuple ((), s)
  // i.e. sets the next state
  def set[S](s: => S): State[S, Unit] = State((_: S) => ((), s))

}
