package workshop.services

import workshop.domain.Domain.{BuyResponse, CreateUser}

trait WorkshopService[F[_]] {
  def buyAllGoods(createUser: CreateUser): F[BuyResponse]
}
