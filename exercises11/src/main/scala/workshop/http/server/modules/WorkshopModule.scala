package workshop.http.server.modules

import cats.effect.{Concurrent, ContextShift, Timer}
import org.http4s.HttpRoutes
import sttp.tapir.generic.auto._
import sttp.tapir.json.circe.jsonBody
import sttp.tapir.server.http4s.{Http4sServerInterpreter, Http4sServerOptions}
import sttp.tapir.{Endpoint, endpoint}
import workshop.domain.Domain.{BuyResponse, CreateUser}
import workshop.http.server.{HttpModule, Response, _}
import workshop.services.WorkshopService

class WorkshopModule[F[_]: Concurrent: ContextShift: Timer](service: WorkshopService[F]) extends HttpModule[F] {
  @SuppressWarnings(Array("Disable.Any"))
  val buyAllEndpoint: Endpoint[CreateUser, Unit, Response[BuyResponse], Any] = ???

  override def httpRoutes(implicit serverOptions: Http4sServerOptions[F]): HttpRoutes[F] =
    Http4sServerInterpreter.toRoutes(List(buyAllEndpoint.handle(service.buyAllGoods)))

  override def endPoints: List[Endpoint[_, Unit, _, _]] =
    List(buyAllEndpoint)
}
