package part2abstractMath

object Semigroups {

  import cats.Semigroup
  import cats.instances.int._

  val naturalIntSemigroup = Semigroup[Int]
  val intCombination = naturalIntSemigroup.combine(2, 46) // addition

  import cats.instances.string._
  val naturalStringSemigroup = Semigroup[String]
  val stringCombination = naturalStringSemigroup.combine("I love ", "Cats") // concatenation

  // specific API
  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemigroup.combine)
  def reduceStrings(list: List[String]): String = list.reduce(naturalStringSemigroup.combine)

  // general API
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(semigroup.combine)

  // TODO  1: support a new type
  case class Expense(id: Long, amount: Double)

  implicit val expenseSemigroup: Semigroup[Expense] = (x: Expense, y: Expense) => Expense(x.id.max(y.id), x.amount + y.amount)

  // extension methods from Semigroup - |+|
  import cats.syntax.semigroup._

  val anIntSum = 2 |+| 3
  // TODO 2: implement reduceThings2 with the |+|
  def reduceThings2[T: Semigroup](list: List[T]): T = list.reduce(_ |+| _)

  def main(args: Array[String]): Unit = {
    println(intCombination)
    println(stringCombination)

    // specific API
    val numbers = (1 to 10).toList
    println(reduceInts(numbers))
    val strings = List("I'm ", "starting ", "to ", "like ", "semigroups")
    println(reduceStrings(strings))

    // general API
    println(reduceThings(numbers))
    println(reduceThings(strings))
    import cats.instances.option._
    val numberOptions = numbers.map(Option(_))
    println(reduceThings(numberOptions))

    val ex1 = Expense(1, 3.0)
    val ex2 = Expense(2, 3.3)

    val combinedExpense = ex1 |+| ex2

    println(expenseSemigroup.combine(ex1, ex2))

    val ex3 = Expense(3, 1.0)
    println(reduceThings(List(ex1, ex2, ex3)))
    println(reduceThings2(List(ex1, ex2, ex3)))
  }

}
