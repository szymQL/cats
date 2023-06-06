package part1recap

object Implicits {

  case class Person(name: String) {
    def greet: String = s"Hi, my name is $name!"
  }

  implicit class ImpersonableString(name: String) {
    def greet: String = Person(name).greet
  }

  val impersonableString = new ImpersonableString("Peter")
  impersonableString.greet

  val greeting = "Peter".greet

  // importing implicit conversions in scope
  import scala.concurrent.duration._
  val oneSec = 1.second

  // implicit conversions and values
  def increment(x: Int)(implicit amount: Int) = x + amount
  implicit val defaultAmount = 10
  val increment2 = increment(2)

  def multiply(x: Int)(implicit times: Int) = x * times
  val times2 = multiply(2)

  // more complex example
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  def listToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String = {
    list.map(serializer.toJson).mkString("[", ",", "]")
  }
  implicit val personSerializer: JSONSerializer[Person] = (person: Person) => {
    s"""
       |{"name": "${person.name}"}
       |""".stripMargin
  }
  val personsJson = listToJson(List(Person("Alice"), Person("Bob")))

  // implicit methods
  implicit def oneArgCaseClassSerializer[T <: Product]: JSONSerializer[T] = (value: T) =>
    s"""
       |{"${value.productElementName(0)}": "${value.productElement(0)}"}
       |""".stripMargin.trim

  case class Cat(name: String)
  val catsToJson = listToJson(List(Cat("Tom"), Cat("Garfield")))

  def main(args: Array[String]): Unit = {
    println(oneArgCaseClassSerializer[Cat].toJson(Cat("Garfield")))
  }

}
