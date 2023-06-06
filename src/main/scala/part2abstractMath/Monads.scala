package part2abstractMath

object Monads {

  val numbersList = List(1, 2, 3)
  val charsList = List('a', 'b', 'c')
  // TODO 1.1: how do you create all combinations of (number, char)?
  val combinationsList = for {
    n <- numbersList
    c <- charsList
  } yield (n, c)

  val numberOption = Option(2)
  val charOption = Option('d')

  // TODO 1.2 create a combination (number, char)
  val optionCombinations = for {
    n <- numberOption
    c <- charOption
  } yield (n, c)

  /*
  Pattern
  - wrapping a value into a monadic value
  - the flatMap mechanism

  MONADS

   */
  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  }

  import cats.Monad
  import cats.instances.option._ // implicit Monad[Option]
  val optionMonad = Monad[Option]
  val anOption = optionMonad.pure(4) // Option(4) == Some(4)
  val aTransformedOption = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None)

  import cats.instances.list._
  val listMonad = Monad[List]
  val aList = listMonad.pure(3)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x + 1))

  def main(args: Array[String]): Unit = {

  }

}
