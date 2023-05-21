package workshop.components

import cats.effect.{Concurrent, ContextShift, Sync}
import cats.syntax.all._
import doobie.util.transactor.Transactor
import io.odin.Logger
import workshop.api.Api
import workshop.api.impl.ApiImpl
import workshop.config.AppConfig
import workshop.db.AppTransactor
import workshop.http.client.RestClient

class IOComponent[F[_]](implicit val api: Api[F], implicit val transactor: Transactor[F])

object IOComponent {
  def apply[F[_]: Sync: Logger: Concurrent: ContextShift](cfg: AppConfig): F[IOComponent[F]] =
    for {
      implicit0(rest: RestClient[F])       <- RestClient.Default[F]
      implicit0(transactor: Transactor[F]) <- AppTransactor[F](cfg.db)
    } yield {
      implicit val api: Api[F] = new ApiImpl[F](cfg.api)

      new IOComponent[F]
    }
}
