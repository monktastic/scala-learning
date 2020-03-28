// https://www.beyondthelines.net/programming/the-free-monad-explained-part-1/

//package free

//import Main.{Console, interaction}

import scala.io.StdIn
import scala.language.higherKinds
import scala.reflect.ClassTag

sealed trait Interact[A]
case class Ask(prompt: String) extends Interact[String]
case class Tell(prompt: String) extends Interact[Unit]

trait Monad[M[_]] {
  def pure[A](a: A): M[A]
  def flatMap[A, B](ma: M[A], f: A => M[B]): M[B]
}

// Natural transformation
trait ~>[F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]
}

object Main {
  type Id[A] = A

  implicit val idMonad = new Monad[Id] {
    def pure[A](a: A): Id[A]                          = a
    def flatMap[A, B](a: Id[A], f: A => Id[B]): Id[B] = f(a)
  }

  sealed trait Free[F[_], A] {
    def flatMap[B, I](f: A => Free[F, B]): Free[F, B] = this match {
      case Return(a) => f(a)
      case Bind(fi: F[I], k) =>
        Bind(fi, k andThen (_ flatMap f))
        // k andThen (_ flatMap f)
        // k andThen (g => g flatMap f)  // (Expanding _)
        // i => (k(i) andThen (g => g flatMap f))  // To simplify the andThen
        // i => (g => g flatMap f)(k(i))  // By defn of andThen
        // i => k(i) flatMap f  // binding 'g'
        // k(_) flatMap f  // Making 'i' anonymous
        // So we want to do simply this, but we have to give _ a type (I) which
        // isn't allowed because of erasure: Bind(fi: F[I], k) doesn't work:
        Bind(fi, k(_: I) flatMap f)
      // The whole thing works by 'inserting' f into the Bind, recursively:
      // Bind(fi, k) flatMap f = Bind(fi, k(_) flatMap f)
    }

    // Bind(Ask("first name"), a => Return(a)) flatMap (first => Bind(Tell("hi, $first"), a => Return(a))
    // Bind(Ask("first name"), i => k(i) flatMap (first => Bind(Tell("hi, $first"), a => Return(a)))
    //  k = a => Return(a)
    // Bind(Ask("first name"), i => Return(i) flatMap (first => Bind(Tell("hi, $first"), a => Return(a))))
    // Bind(Ask("first name"), i => Bind(Tell("hi, $i"), a => Return(a)))

    def map[B](f: A => B): Free[F, B] = flatMap(a => Return(f(a)))

    // F = compile time language (e.g Interact)
    // G = runtime language (e.g. Id)
    // this version is not stack safe (but possible to write it in tail recursive way)
    def foldMap[G[_]](f: F ~> G)(implicit monadG: Monad[G]): G[A] = this match {
      case Return(a) => monadG.pure(a)
      case Bind(fi, k) => monadG.flatMap(f(fi), k(_: Any) foldMap f)
    }

    // Simplified version for Id.
    def foldMapId(f: F ~> Id): Id[A] = this match {
      case Return(a) => a
      case Bind(fi, k) => k(f(fi)).foldMapId(f)
    }
  }

  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Bind[F[_], I, A](fi: F[I], k: I => Free[F, A]) extends Free[F, A]

  object Console extends (Interact ~> Id) {
    def apply[A](i: Interact[A]) = i match {
      case Ask(prompt) =>
        print(prompt + " ")
        StdIn.readLine
      case Tell(prompt) =>
        println(prompt)
    }
  }

  implicit def lift[F[_], A](fa: F[A]): Free[F, A] = Bind(fa, (a: A) => Return(a))

  val interaction: Free[Interact, Unit] =
    for {
      first <- Ask("First name?")
      last <- Ask("Last name?")
      _ <- Tell(s"Hi, $first $last!")
    } yield ()


  def main(args: Array[String]): Unit = {
    //    interaction.foldMap(Console)
    interaction.foldMapId(Console)
  }
}
