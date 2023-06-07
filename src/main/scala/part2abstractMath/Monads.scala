package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

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
    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => pure(f(a)))
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

  import cats.instances.future._

  // TODO 2: use a Monad[Future]
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val futureMonad = Monad[Future]
  val aFuture = futureMonad.pure(42)
  val aTransformedFuture = aFuture.flatMap(value => futureMonad.pure(value + 1))

  // specialized API
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))
  def getPairsOption(numbers: Option[Int], chars: Option[Char]): Option[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))
  def getPairsFuture(numbers: Future[Int], chars: Future[Char]): Future[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))

  // generalize
  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] = monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  // extension methods - weirder imports - pure, flatmap
  import cats.syntax.applicative._
  val oneOption = 1.pure[Option] // implicit Monad[Option] will be used => Some(1)

  import cats.syntax.flatMap._
  val oneOptionTransformed = oneOption.flatMap(x => (x + 1).pure[Option])

  // TODO 3: implement map method in MyMonad
  // Monads extend Functors

  //for-comprehensions
  val composedOptionFor = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two

  import cats.syntax.functor._
  def getPairsFor[M[_]: Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] = for {
    a <- ma
    b <- mb
  } yield (a, b)

  def main(args: Array[String]): Unit = {
    println(getPairs(numbersList, charsList))
    println(getPairs(numberOption, charOption))
  }

}
