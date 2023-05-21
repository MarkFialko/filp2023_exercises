package workshop.db

import java.util.concurrent.Executors

import workshop.config.DbConfig
import cats.effect.{Async, Blocker, ContextShift}
import com.zaxxer.hikari.HikariDataSource
import doobie.hikari.HikariTransactor

import scala.concurrent.ExecutionContext

object AppTransactor {
  def apply[F[_]: Async: ContextShift](cfg: DbConfig): F[HikariTransactor[F]] =
    Async[F].delay {
      val connectionPool = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(cfg.transactionPoolSize))

      val transactionPool = ExecutionContext.fromExecutor(Executors.newCachedThreadPool { (r: Runnable) =>
        val t = new Thread(r, "workshop-transaction-pool")
        t.setDaemon(true)
        t
      })

      val dataSource = new HikariDataSource()

      dataSource.setJdbcUrl(cfg.url)
      dataSource.setUsername(cfg.user)
      dataSource.setPassword(cfg.password)
      dataSource.setMaximumPoolSize(cfg.connectionPoolSize)
      dataSource.setConnectionInitSql("SET TIME ZONE 'UTC'")

      HikariTransactor[F](dataSource, connectionPool, Blocker.liftExecutionContext(transactionPool))
    }
}
