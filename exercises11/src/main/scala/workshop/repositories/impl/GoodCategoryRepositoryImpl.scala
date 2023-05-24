package workshop.repositories.impl

import cats.effect.Sync
import doobie.Query0
import doobie.util.transactor.Transactor
import workshop.domain.Domain._
import workshop.repositories.GoodCategoryRepository
import doobie.syntax.all._

class GoodCategoryRepositoryImpl[F[_]: Sync](transactor: Transactor[F]) extends GoodCategoryRepository[F] {
  def userCategoryId(userId: Id): F[Id] =
    GoodsCategorySql.userCategoryId(userId).unique.transact(transactor)
}

object GoodsCategorySql {

  import workshop.db.Instances._
  def userCategoryId(userId: Id): Query0[Id] =
    sql"select category_id from user_goods_category_id where user_id = $userId".query[Id]
}
