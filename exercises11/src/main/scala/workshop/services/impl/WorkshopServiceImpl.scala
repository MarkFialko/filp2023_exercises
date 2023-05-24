package workshop.services.impl

import cats.Monad
import workshop.domain.Domain._
import workshop.repositories.{GoodCategoryRepository, GoodsRepository, UserRepository}
import workshop.services.WorkshopService

import cats.syntax.all._

class WorkshopServiceImpl[F[_]: Monad](
    implicit goodCategoryRepo: GoodCategoryRepository[F],
    userRepo: UserRepository[F],
    goodsRepo: GoodsRepository[F]
) extends WorkshopService[F] {
  def buyAllGoods(createUser: CreateUser): F[BuyResponse] =
    for {
      token <- userRepo.createUser(createUser)
      user <- userRepo.getUser(token)
      goods <- goodsRepo.list(token)
      categoryId <- goodCategoryRepo.userCategoryId(user.id)
      buyRequest = BuyRequest(goodsToBuy(goods, categoryId, user.balance))
      res <- goodsRepo.buy(token, buyRequest)
    } yield res

  private def goodsToBuy(goods: List[Good], categoryId: Id, balance: Double): List[Id] = {
    goods
      .foldLeft((balance, List.empty[Id])) {
        case ((b, l), elem) if elem.category.id == categoryId && b >= elem.price => (b - elem.price, l :+ elem.id)
        case ((b, l), _)                                                         => (b, l)
      }
      ._2
  }
}
