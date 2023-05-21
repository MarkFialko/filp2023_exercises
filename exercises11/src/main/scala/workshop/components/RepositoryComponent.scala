package workshop.components

import cats.effect.Sync
import workshop.repositories._
import workshop.repositories.impl.{GoodCategoryRepositoryImpl, GoodsRepositoryImpl, UserRepositoryImpl}

class RepositoryComponent[F[_]](
    implicit val goodCategoryRepo: GoodCategoryRepository[F],
    implicit val goodsRepo: GoodsRepository[F],
    implicit val userRepo: UserRepository[F]
)

object RepositoryComponent {
  def apply[F[_]: Sync](implicit io: IOComponent[F]): F[RepositoryComponent[F]] =
    Sync[F].delay {
      import io._

      implicit val goodCategoryRepo: GoodCategoryRepository[F] = new GoodCategoryRepositoryImpl[F](io.transactor)
      implicit val goodsRepo: GoodsRepository[F]               = new GoodsRepositoryImpl[F]
      implicit val userRepo: UserRepository[F]                 = new UserRepositoryImpl[F]

      new RepositoryComponent[F]
    }
}
