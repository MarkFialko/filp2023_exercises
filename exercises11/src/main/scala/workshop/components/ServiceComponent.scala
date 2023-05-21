package workshop.components

import cats.effect.Sync
import workshop.services.WorkshopService
import workshop.services.impl.WorkshopServiceImpl

class ServiceComponent[F[_]](implicit val workshopService: WorkshopService[F])

object ServiceComponent {
  def apply[F[_]: Sync](implicit repoComp: RepositoryComponent[F]): F[ServiceComponent[F]] =
    Sync[F].delay {
      import repoComp._

      implicit val workshopService: WorkshopService[F] = new WorkshopServiceImpl[F]

      new ServiceComponent[F]
    }
}
