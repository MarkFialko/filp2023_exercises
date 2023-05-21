package workshop.components

import java.net.InetSocketAddress

import cats.effect.{ConcurrentEffect, ContextShift, Resource, Timer}
import cats.syntax.all._
import org.http4s.HttpRoutes
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.{Router, Server}
import org.http4s.syntax.kleisli._
import sttp.tapir.docs.openapi.OpenAPIDocsInterpreter
import sttp.tapir.openapi.OpenAPI
import sttp.tapir.openapi.circe.yaml._
import sttp.tapir.server.http4s.Http4sServerOptions
import sttp.tapir.swagger.http4s.SwaggerHttp4s
import workshop.config.ServerConfig
import workshop.http.server.HttpModule

import scala.concurrent.ExecutionContext

case class ServerComponent(address: InetSocketAddress)

object ServerComponent {
  def build[F[_]: ConcurrentEffect: Timer: ContextShift](modules: List[HttpModule[F]])(
      httpConfig: ServerConfig,
      ec: ExecutionContext
  ): Resource[F, ServerComponent] = {
    implicit val serverOptions: Http4sServerOptions[F] = Http4sServerOptions.default[F]

    def bind(httpModules: List[HttpModule[F]], port: Int): Resource[F, Server[F]] = {
      val docs: OpenAPI =
        OpenAPIDocsInterpreter.toOpenAPI(httpModules.flatMap(_.endPoints), "Workshop", "1.0")
      val swagger = new SwaggerHttp4s(docs.toYaml)

      BlazeServerBuilder[F](ec)
        .bindHttp(port, "0.0.0.0")
        .withHttpApp(
          Router(
            "/" -> (httpModules.foldLeft(HttpRoutes.empty[F])((acc, r) => acc <+> r.httpRoutes) <+> swagger.routes)
          ).orNotFound
        )
        .resource
    }
    for {
      server <- bind(modules, httpConfig.port)
    } yield ServerComponent(server.address)
  }
}
