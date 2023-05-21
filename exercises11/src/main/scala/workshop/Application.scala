package workshop

import cats.effect.{ExitCode, IO, IOApp, Resource}
import io.odin._
import workshop.components.{IOComponent, RepositoryComponent, ServerComponent, ServiceComponent}
import workshop.config.AppConfig
import workshop.http.server.modules.WorkshopModule

import scala.concurrent.ExecutionContext

/**
  * Сенсация!! Открылся интернет магазин уникальных вещей
  * и вам очень нужны товары определенной категории
  *
  * Ваша задача зайти в магазин и начать скупать все товары, которые вам попадаются на пути,
  * пока достаточно денег, если попадается товар дороже чем, остаток, то его надо пропустить
  *
  * К счастью у магазина есть API и мы как программисты просто обязаны автоматизировать все действия
  * http://filp.ulanzetz.com/workshop/swagger
  *
  * После запуска приложения у вас будет доступен локально сваггер для вашего сервиса
  * http://127.0.0.1:8080/docs
  *
  * Где нужно ввести Имя, Фамилию, Группу и попробовать свою программу автоматизации покупок
  *
  * Если всё получилось, то вам вернется статус BuyResponseStatus.Success
  *
  * Что должна сделать программа
  * - пройти аутентификацию на сервисе
  * - запросить информацию о доступном остатке в кошельке
  * - получить список товаров
  * - отобрать товары подходящие вам по категории и по стоимости, проходить надо последовательно, в порядке получения
  * - отправить список покупок
  */
object Application extends IOApp {
  implicit val logger: Logger[IO] = consoleLogger()

  val mainApp: Resource[IO, Unit] =
    for {
      cfg                                      <- Resource.eval(AppConfig.load[IO])
      implicit0(io: IOComponent[IO])           <- Resource.eval(IOComponent[IO](cfg))
      implicit0(repo: RepositoryComponent[IO]) <- Resource.eval(RepositoryComponent[IO])
      implicit0(service: ServiceComponent[IO]) <- Resource.eval(ServiceComponent[IO])
      server <- ServerComponent.build[IO](
        List(new WorkshopModule[IO](service.workshopService))
      )(cfg.server, executionContext)
      _ <- Resource.eval(logger.info(s"Server started at ${server.address}"))
    } yield ()

  def run(args: List[String]): IO[ExitCode] =
    mainApp.use(_ => IO.never).as(ExitCode.Success)
}
