package workshop.services.impl

import cats.Monad
import workshop.domain.Domain._
import workshop.repositories.{GoodCategoryRepository, GoodsRepository, UserRepository}
import workshop.services.WorkshopService

class WorkshopServiceImpl[F[_]: Monad](
    implicit goodCategoryRepo: GoodCategoryRepository[F],
    userRepo: UserRepository[F],
    goodsRepo: GoodsRepository[F]
) extends WorkshopService[F] {
  def buyAllGoods(createUser: CreateUser): F[BuyResponse] = ???
}
