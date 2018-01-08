package monads

import cats.Monoid


/**
  * Created by aditpras on 5/6/17.
  */
case class Writer[W : Monoid, A](w: W, a: A) {
//  def flatMap[B](f: A => Writer[W, B]): Writer[W, B] = { f(pair._2) }

  def flatMap[B](f: A => Writer[W, B]): Writer[W, B] = {
    val wb = f(a)
    (Monoid[W].combine(w, wb.w), wb.a)
  }

  def map[B](f: A => B): Writer[W, B] = Writer(w, f(a))

  def run: (W, A) = (w, a)
}

object Writer {
  implicit def toWriter[W, A](tuple: (W, A)): Writer[W, A] = Writer(tuple._1, tuple._2)

  def flatten[W : Monoid, A](wwa: Writer[W, Writer[W, A]]): Writer[W, A] =
    Writer(Monoid[W].combine(wwa.w, wwa.a.w), wwa.a.a)
}
