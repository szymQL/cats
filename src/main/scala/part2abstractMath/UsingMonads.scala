package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object UsingMonads {

  import cats.Monad
  import cats.instances.list._
  import cats.instances.option._

  val monadList = Monad[List]
  val aSimpleList = monadList.pure(2)
  val anExtendedList = monadList.flatMap(aSimpleList)(x => List(x, x + 1))
  // applicable to Option, Try, Future

  // either is also a monad
  val aManualEither: Either[String, Int] = Right(42)

  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either._

  val loadingMonad = Monad[LoadingOr]
  val anEither = loadingMonad.pure(45) // LoadingOr[Int] == Right(45)
  val aChangedLoading = loadingMonad.flatMap(anEither)(n => if (n % 2 == 0) Right(n + 1) else Left("Loading"))

  // imaginary online store
  case class OrderStatus(orderId: Long, status: String)

  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] = Right(OrderStatus(orderId, "Ready to ship"))

  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000) Left("Not available yet, refreshing data...")
    else Right("Amsterdam, NL")

  val orderId = 457L
  val orderLocation = loadingMonad.flatMap(getOrderStatus(orderId))(orderStatus => trackLocation(orderStatus))
  // use extension methods

  import cats.syntax.flatMap._
  import cats.syntax.functor._

  val orderLocationBetter = getOrderStatus(orderId).flatMap(trackLocation)
  val orderLocationFor = for {
    orderStatus <- getOrderStatus(orderId)
    location <- trackLocation(orderStatus)
  } yield location

  // TODO the service layer API for a web app
  case class Connection(host: String, port: String)
  val config = Map(
    "host" -> "localhost",
    "port" -> "8080"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }
  // DO NOT CHANGE THE CODE
  /*
  Requirements:
      - if the host and port are found in the configuration map, then we'll return a M containing a connection with those values
        otherwise the method will fail, according to the logic of the type M
        (for Try it will return a Failure, for Option it will return None, for Future it will be a failed Future, for Either it will return a Left)
      - the issueRequest method returns a M containing the string: "request (payload) has been accepted", if the payload is less than 20 characters
        otherwise the method will fail, according to the logic of the type M

  TODO: provide a real implementation of HttpService using Try, Option, Future, Either
   */

  object TryHttpService extends HttpService[Try] {
    override def getConnection(cfg: Map[String, String]): Try[Connection] = {
      val connOpt = for {
        host <- cfg.get("host")
        port <- cfg.get("port")
      } yield {
        Connection(host, port)
      }
      Try(connOpt.get)
    }

    override def issueRequest(connection: Connection, payload: String): Try[String] = {
      if (payload.length < 20) Success("request has been accepted")
      else Failure(new Exception("request not accepted"))
    }
  }

  object OptionHttpService extends HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] = {
      for {
        host <- cfg.get("host")
        port <- cfg.get("port")
      } yield {
        Connection(host, port)
      }
    }

    override def issueRequest(connection: Connection, payload: String): Option[String] = {
      if (payload.length < 20) Some("request has been accepted")
      else None
    }
  }

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  object FutureHttpService extends HttpService[Future] {
    override def getConnection(cfg: Map[String, String]): Future[Connection] = {
      val connOpt = for {
        host <- cfg.get("host")
        port <- cfg.get("port")
      } yield {
        Connection(host, port)
      }
      connOpt match {
        case Some(value) => Future(value)
        case None => Future.failed(new Exception("request not accepted"))
      }
    }

    override def issueRequest(connection: Connection, payload: String): Future[String] = {
      if (payload.length < 20) Future("request has been accepted")
      else Future.failed(new Exception("request has not been accepted"))
    }
  }

  object EitherHttpService extends HttpService[ErrorOr] {
    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] = {
      val connOpt = for {
        host <- cfg.get("host")
        port <- cfg.get("port")
      } yield {
        Connection(host, port)
      }
      connOpt match {
        case Some(value) => Right(value)
        case None => Left(new Exception("request not accepted"))
      }
    }

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] = {
      if (payload.length < 20) Right("request has been accepted")
      else Left(new Exception("request has not been accepted"))
    }
  }

  def getResponse[M[_]: Monad](service: HttpService[M], payload: String): M[String] = {
    for {
      conn <- service.getConnection(config)
      response <- service.issueRequest(conn, payload)
    } yield response
  }

  def main(args: Array[String]): Unit = {
    println(getResponse(OptionHttpService, "Hello Option"))
  }

}
