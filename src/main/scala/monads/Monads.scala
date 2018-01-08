package monads

//import cats.data.{Reader, State, Writer}
//import cats.data.State

import cats.implicits._


/**
  * Created by aditpras on 5/6/17.
  */
object Monads {
  def main(args: Array[String]): Unit = {
    val r = for {
      v <- r1(4)
      s <- r2(v, "got")
    } yield (v, s)

    r1(4).flatMap(v => r2(v, "got"))

    val rRes = r.run(DB(5))
    println(rRes)
    assert(rRes._1 == 9)
    assert(rRes._2 == "got 9")

    val w = for {
      s <- w1(1)
      f <- w2("other")
    } yield (s, f)

    val wRes = w.run
    println(wRes)
    assert(wRes._1 == 3)

    val s = for {
      f <- s1
      s <- s2
    } yield (f, s)

    val sRes = s.run(Holder(2)).value
    println(sRes)
    assert(sRes._1 == Holder(5))
    assert(sRes._2 == (0.234f, "another op"))
  }

  /*
    State monad
   */
  case class Holder(x: Int)

  def s1 = State[Holder, Float] { h =>
    (h.copy(h.x + 1), 0.234f)
  }

  def s2 = State[Holder, String] { h =>
    (h.copy(h.x + 2), "another op")
  }

  /*
    Writer monad
   */
  def w1(x: Int): Writer[Int, String] = Writer(1, s"foo $x")

  def w2(s: String): Writer[Int, Float] = Writer(2, 0.123f)

  /*
    Reader monad
   */
  case class DB(x: Int) {
    def get = x
  }

  def r1(x: Int) = Reader { db: DB =>
    db.get + x
  }

  def r2(v: Int, x: String) = Reader { db: DB =>
    s"$x ${db.get + v}"
  }
}

