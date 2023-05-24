package workshop.repositories.impl

import cats.syntax.all._
import workshop.api.Api
import workshop.domain.Domain._
import workshop.repositories.GoodsRepository

class GoodsRepositoryImpl[F[_]: Api] extends GoodsRepository[F] {
  def list(token: Token): F[List[Good]] =
    Api[F].get(_.addPath("goods", "list"), token.some)

  def buy(token: Token, req: BuyRequest): F[BuyResponse] =
    Api[F].post[BuyRequest, BuyResponse](_.addPath("goods", "buy"), req, token.some)
}
