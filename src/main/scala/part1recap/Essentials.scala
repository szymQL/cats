package part1recap

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object Essentials {

  // values
  val aBoolean: Boolean = false

  // expressions are EVALUATED to a value
  val anIfExpression: String = if (2 > 3) "bigger" else "smaller"

  // instructions vs expressions
  val theUnit: Unit = println("Hello, Scala")

  // OOP
  class Animal

  class Cat extends Animal

  trait Carnivore {
    def eat(animal: Animal): Unit
  }

  // inheritance
  class Crocodile extends Animal with Carnivore {
    override def eat(animal: Animal): Unit = println("Chomp")
  }

  // singleton
  object MySingleton

  // companions
  object Carnivore

  // generics
  class MyList[A]

  // method notation
  val three = 1 + 2
  val anotherThree = 1.+(2)

  // functional programming
  val incrementer: Int => Int = x => x + 1
  val incremented = incrementer(45) // 46

  // HOF
  val processedList = List(1, 2, 3).map(incrementer)
  val aLongerList = List(1, 2, 3).flatMap(x => List(x, x + 1)) // List(1, 2, 2, 3, 3, 4)

  // for-comprehensions
  val checkerboard = List(1, 2, 3).flatMap(n => List('a', 'b', 'c').map(c => (n, c)))
  val anotherCheckerboard = for {
    n <- List(1, 2, 3)
    c <- List('a', 'b', 'c')
  } yield (n, c)

  // option and try
  val anOption: Option[Int] = Option(3) // Some(3)
  val doubledOption: Option[Int] = anOption.map(_ * 2)

  val anAttempt: Try[Int] = Try(42) // Success(42)
  val aModifiedAttempt: Try[Int] = anAttempt.map(_ + 10)

  //pattern matching
  val anUnknown: Any = 45
  val ordinal = anUnknown match {
    case 1 => "first"
    case 2 => "second"
    case _ => "unknown"
  }

  val optionDescription = anOption match {
    case Some(value) => s"the option is not empty: $value"
    case None => "the option is empty"
  }

  // Futures
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val aFuture = Future {
    42
  }

  // wait for completion (async)
  aFuture.onComplete {
    case Success(value) => println(s"The async meaning of life is $value")
    case Failure(exception) => println(s"Meaning of value failed $exception")
  }

  // map a Future
  val anotherFuture = aFuture.map(_ + 1)

  // partial functions
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 43
    case 8 => 56
    case 100 => 999
  }

  //some more advanced stuff
  trait HigherKindedType[F[_]]
  trait SequenceChecker[F[_]] {
    def isSequential: Boolean
  }
  val listChecker = new SequenceChecker[List] {
    override def isSequential = true
  }

  def main(args: Array[String]): Unit = {

  }

}
