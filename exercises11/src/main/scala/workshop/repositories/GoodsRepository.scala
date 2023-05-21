package workshop.repositories

import workshop.domain.Domain._

trait GoodsRepository[F[_]] {
  def list(token: Token): F[List[Good]]

  def buy(token: Token, req: BuyRequest): F[BuyResponse]
}
