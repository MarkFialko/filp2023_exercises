package workshop

import cats.effect.Sync
import com.typesafe.config.ConfigFactory
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

package object config {
  case class AppConfig(api: ApiConfig, db: DbConfig, server: ServerConfig)

  object AppConfig {
    def load[F[_]: Sync]: F[AppConfig] =
      Sync[F].delay(ConfigFactory.load("application.conf").resolve().as[AppConfig])
  }

  case class ApiConfig(endpoint: String)

  case class DbConfig(url: String, user: String, password: String, connectionPoolSize: Int, transactionPoolSize: Int)

  case class ServerConfig(port: Int)
}
