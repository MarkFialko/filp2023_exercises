package workshop.repositories.impl

import cats.effect.{ContextShift, IO}
import doobie.Transactor
import doobie.implicits._
import doobie.util.ExecutionContexts
import doobie.util.fragment.Fragment
import org.specs2.mutable.Specification
import workshop.domain.Domain.Id

class GoodCategoryRepositoryImplTest extends Specification with doobie.specs2.IOChecker {
  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContexts.synchronous)

  def transactor: Transactor[IO] = {
    val tx = Transactor.fromDriverManager[IO](
      "org.h2.Driver",
      "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1",
      "",
      ""
    )
    createSql.update.run
      .transact(tx)
      .unsafeRunSync()
    tx
  }

  val createSql: Fragment = sql"""create table user_goods_category_id
                                 |(
                                 |	user_id varchar not null,
                                 |	category_id varchar not null
                                 |);
                                 |""".stripMargin

  check(GoodsCategorySql.userCategoryId(Id("1")))
}
