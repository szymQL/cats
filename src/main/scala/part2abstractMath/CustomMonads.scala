package part2abstractMath

import scala.annotation.tailrec

object CustomMonads {

  import cats.Monad

  implicit object OptionMonad extends Monad[Option] {
    override def pure[A](x: A): Option[A] = Option(x)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    // tailRecM must NOT stack-overflow
    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case None => None
      case Some(Left(v)) => tailRecM(v)(f)
      case Some(Right(b)) => Some(b)
    }
  }

  // TODO 1: define a monad for the identity type

  type Identity[T] = T

  implicit object IdentityMonad extends Monad[Identity] {
    override def pure[A](x: A): Identity[A] = ???

    override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = ???

    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = ???
  }

  def main(args: Array[String]): Unit = {

  }

}
