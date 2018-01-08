package monads

import cats.{Eval, Now}

/**
  * Created by aditpras on 5/6/17.
  */
case class State[S, A](sf: S => (S, A)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s: S =>
    val (s2, a) = sf(s)
    f(a).sf(s2)
  }

  def map[B](f: A => B): State[S, B] = State { s: S =>
    val (s2, a) = sf(s)
    (s2, f(a))
  }

  def run(s: S): Eval[(S, A)] = Now(sf(s))
}

object State {
  def flatten[S, A](ssa: State[S, State[S, A]]): State[S, A] = State { s: S =>
    val (s2, sa) = ssa.sf(s)
    sa.sf(s2)
  }
}
