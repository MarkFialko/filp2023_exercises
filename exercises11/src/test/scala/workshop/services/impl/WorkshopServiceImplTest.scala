package workshop.services.impl

import cats.effect.IO
import org.scalatest.Assertion
import org.scalatest.wordspec.AnyWordSpec
import workshop.domain.Domain._
import workshop.repositories.{GoodCategoryRepository, GoodsRepository, UserRepository}

class WorkshopServiceImplTest extends AnyWordSpec {
  final val createUser1 = CreateUser("name1", "surname1", "group1")
  final val token1      = Token("token1")
  final val token2      = Token("token2")
  final val userId1     = Id("3")
  final val user1       = User(userId1, "name1", "surname1", "group1", 1000.0)
  final val categoryId1 = Id("1")

  final val good1 = Good(Id("1"), "good1", "descr1", GoodCategory(Id("1"), "Одежда"), 500.0)
  final val good2 = Good(Id("2"), "good1", "descr2", GoodCategory(Id("2"), "Одежда"), 400.0)
  final val good3 = Good(Id("3"), "good1", "descr3", GoodCategory(Id("3"), "Одежда"), 300.0)
  final val good4 = Good(Id("4"), "good1", "descr4", GoodCategory(Id("1"), "Одежда"), 600.0)
  final val good5 = Good(Id("5"), "good1", "descr5", GoodCategory(Id("1"), "Одежда"), 100.0)
  final val good6 = Good(Id("5"), "good1", "descr5", GoodCategory(Id("1"), "Одежда"), 1100.0)

  "WorkshopService" should {
    "empty goods" in {
      test(Nil, BuyRequest(Nil))
    }
    "empty category goods" in {
      test(List(good2, good3), BuyRequest(Nil))
    }
    "not enough money" in {
      test(List(good6), BuyRequest(Nil))
    }
    "one goods" in {
      test(List(good1), BuyRequest(List(Id("1"))))
    }
    "goods with skip" in {
      test(List(good1, good2, good3, good4, good5), BuyRequest(List(Id("1"), Id("5"))))
    }
  }

  private def test(goods: List[Good], buyRequest: BuyRequest): Assertion = {
    implicit val goodCategoryRepository: GoodCategoryRepository[IO] = new GoodCategoryRepository[IO] {
      override def userCategoryId(userId: Id): IO[Id] =
        if (userId == userId1)
          IO.pure(categoryId1)
        else
          IO.raiseError(new RuntimeException("User not found"))
    }

    implicit val userRepository: UserRepository[IO] = new UserRepository[IO] {
      override def createUser(createUser: CreateUser): IO[Token] =
        if (createUser === createUser1)
          IO.pure(token1)
        else
          IO.pure(token2)

      override def getUser(token: Token): IO[User] =
        if (token == token1)
          IO.pure(user1)
        else
          IO.raiseError(new RuntimeException("user not found"))

    }
    implicit val goodsRepository: GoodsRepository[IO] = new GoodsRepository[IO] {
      override def list(token: Token): IO[List[Good]] =
        if (token === token1)
          IO.pure(goods)
        else
          IO.raiseError(new RuntimeException("user not found"))

      override def buy(token: Token, req: BuyRequest): IO[BuyResponse] =
        if (token === token1 && req === buyRequest)
          IO.pure(BuyResponse(BuyResponseStatus.Success))
        else
          IO.pure(BuyResponse(BuyResponseStatus.IncorrectGoods))
    }
    val service = new WorkshopServiceImpl[IO]

    val assertation = for {
      result <- service.buyAllGoods(createUser1)
    } yield assert(result === BuyResponse(BuyResponseStatus.Success))

    assertation.unsafeRunSync()

  }
}
