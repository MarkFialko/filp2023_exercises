package workshop.repositories

import workshop.domain.Domain._

trait GoodCategoryRepository[F[_]] {
  def userCategoryId(userId: Id): F[Id]
}
