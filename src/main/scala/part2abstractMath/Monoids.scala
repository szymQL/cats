package part2abstractMath

object Monoids {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._

  val numbers = (1 to 1000).toList
  // |+| is always assiociative
  val sumLeft = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)

  // define a general API
  // def combineFold[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.foldLeft()(_ |+| _)

  // MONOIDS

  import cats.Monoid

  val intMonoid = Monoid[Int]
  val combineInt = intMonoid.combine(23, 999)
  val zero = intMonoid.empty // 0

  import cats.instances.string._

  val stringMonoid = Monoid[String]
  val combineString = stringMonoid.combine("yes, ", "of course")
  val emptyString = stringMonoid.empty // "" - empty string

  import cats.instances.option._ // construct and implicit Monoid[Option[Int]]

  val optionMonoid = Monoid[Option[Int]]
  val emptyOption = optionMonoid.empty
  val combineOption = optionMonoid.combine(Option(2), Option.empty[Int])

  // extension methods for Monoids - |+|
  // import cats.syntax.monoid._
  val combinedOptionFancy = Option(3) |+| Option(7)

  // TODO 1: implement a combineFold
  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T = list.foldLeft(monoid.empty)(_ |+| _)

  // TODO 2: combine a list of phonebooks as Maps[String, Int]
  val phonebooks = List(
    Map(
      "Alice" -> 235,
      "Bob" -> 647
    ),
    Map(
      "Charlie" -> 372,
      "Daniel" -> 889
    ),
    Map(
      "Tina" -> 123
    )
  )

  import cats.instances.map._

  // TODO 3: shopping cart and online stoers with Monoids
  case class ShoppingCart(items: List[String], total: Double)

  implicit val cartMonoid: Monoid[ShoppingCart] = Monoid.instance[ShoppingCart](
    ShoppingCart(List.empty[String], 0.0),
    (cart1, cart2) => ShoppingCart(cart1.items ++ cart2.items, cart1.total + cart2.total)
  )

  def checkout(carts: List[ShoppingCart]): ShoppingCart = combineFold(carts)

  def main(args: Array[String]): Unit = {
    println(combineFold(numbers))
    println(combineFold(phonebooks))
    println(checkout(List(
      ShoppingCart(List("iphone", "shoes"), 799.0),
      ShoppingCart(List("TV"), 20000.0),
      ShoppingCart(List(), 0)
    )))
  }

}
