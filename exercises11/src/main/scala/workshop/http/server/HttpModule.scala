package workshop.http.server

import org.http4s.HttpRoutes
import sttp.tapir.Endpoint
import sttp.tapir.server.http4s.Http4sServerOptions

trait HttpModule[F[_]] {
  def httpRoutes(implicit serverOptions: Http4sServerOptions[F]): HttpRoutes[F]

  def endPoints: List[Endpoint[_, Unit, _, _]]
}
