package workshop.repositories.impl

import workshop.api.Api
import workshop.domain.Domain._
import workshop.repositories.GoodsRepository

class GoodsRepositoryImpl[F[_]: Api] extends GoodsRepository[F] {
  def list(token: Token): F[List[Good]] = ???

  def buy(token: Token, req: BuyRequest): F[BuyResponse] = ???
}
