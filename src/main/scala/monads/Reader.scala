package monads


/**
  * Created by aditpras on 5/6/17.
  */
case class Reader[R, A](rf: R => A) {

  // Reader[R, A] -> function from R to A
  // function: A -> Reader[R, B]
  // Reader[R, B]

  // r: R =>
  //   a = rf(r)
  //   rb = f(a)
  //   result = rb.rf(r)
  //   Reader { r =>  result }

  def flatMap[B](f: A => Reader[R, B]): Reader[R, B] = Reader { r: R =>
    val a = rf(r)
    val rb = f(rf(r))
    val result = rb.rf(r)
    result
  }


  def flatMap[B](f: A => Reader[R, B]): Reader[R, B] = Reader { r: R =>
    f(rf(r)).rf(r)
  }

  def map[B](f: A => B): Reader[R, B] = Reader { r: R =>
    f(rf(r))
  }

  def run(r: R) = rf(r)
}

object Reader {
  // Because of this we can remove the "Reader" wrappers above.
  implicit def toReader[R, A](rf: R => A): Reader[R, A] = Reader(rf)

  def flatten[R, A](rr: Reader[R, Reader[R, A]]): Reader[R, A] = { r: R =>
    rr.rf(r).rf(r)
  }
}
