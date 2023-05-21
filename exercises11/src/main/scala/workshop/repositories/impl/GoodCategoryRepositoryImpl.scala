package workshop.repositories.impl

import cats.effect.Sync
import doobie.Query0
import doobie.util.transactor.Transactor
import workshop.domain.Domain._
import workshop.repositories.GoodCategoryRepository

class GoodCategoryRepositoryImpl[F[_]: Sync](transactor: Transactor[F]) extends GoodCategoryRepository[F] {
  def userCategoryId(userId: Id): F[Id] = ???
}

object GoodsCategorySql {
  def userCategoryId(userId: Id): Query0[Id] = ???
}
