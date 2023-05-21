package workshop.repositories.impl

import cats.effect.{ContextShift, IO}
import cats.syntax.option._
import doobie.util.ExecutionContexts
import io.odin.Logger
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec
import sttp.client3.asynchttpclient.cats.AsyncHttpClientCatsBackend
import sttp.model.{HeaderNames, Method, Uri}
import workshop.api.Api
import workshop.api.impl.ApiImpl
import workshop.config.ApiConfig
import workshop.domain.Domain
import workshop.domain.Domain._
import workshop.http.client.RestClient

class GoodsRepositoryImplTest extends AnyWordSpec {
  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContexts.synchronous)
  implicit val logger: Logger[IO]   = Logger.noop[IO]

  "GoodsRepository" should {
    "list goods" in {
      val backend = AsyncHttpClientCatsBackend
        .stub[IO]
        .whenRequestMatches(request =>
          request.method === Method.GET &&
            request.uri === Uri.unsafeParse("http://test.local/goods/list") &&
            request.header(HeaderNames.Authorization) === "Bearer token1".some
        )
        .thenRespond("""
            |{
            |  "payload": [
            |    {
            |      "id": "61",
            |      "name": "Белая футболка",
            |      "description": "100% хлопок",
            |      "category": {
            |        "id": "6",
            |        "name": "Одежда"
            |      },
            |      "price": 500
            |    },
            |    {
            |      "id": "1",
            |      "name": "Бананы",
            |      "description": "Самые лучшие бананы прямиком из Марокко",
            |      "category": {
            |        "id": "1",
            |        "name": "Продукты питания"
            |      },
            |      "price": 100
            |    }
            |  ],
            |  "responseTime": 143,
            |  "time": "2021-05-19T11:28:42.683Z",
            |  "status": "Ok"
            |}""".stripMargin)
      implicit val restClient: RestClient.Default[IO] = new RestClient.Default[IO](backend)
      implicit val api: Api[IO]                       = new ApiImpl[IO](ApiConfig(endpoint = "http://test.local"))
      val repository                                  = new GoodsRepositoryImpl[IO]

      val expects = List(
        Good(Domain.Id("61"), "Белая футболка", "100% хлопок", GoodCategory(Domain.Id("6"), "Одежда"), 500.0),
        Good(
          Domain.Id("1"),
          "Бананы",
          "Самые лучшие бананы прямиком из Марокко",
          GoodCategory(
            Domain.Id("1"),
            "Продукты питания"
          ),
          100.0
        )
      )
      val assertation = for {
        goods <- repository.list(Token("token1"))
      } yield goods should contain theSameElementsAs expects

      assertation.unsafeRunSync()
    }
  }

  "GoodsRepository" should {
    "buy goods" in {
      val backend = AsyncHttpClientCatsBackend
        .stub[IO]
        .whenRequestMatches(request =>
          request.method === Method.POST &&
            request.uri === Uri.unsafeParse("http://test.local/goods/buy") &&
            request.header(HeaderNames.Authorization) === "Bearer token1".some
        )
        .thenRespond("""
                       |{
                       |  "payload":{
                       |    "status": "Success"
                       |  },
                       |  "responseTime": 143,
                       |  "time": "2021-05-19T11:28:42.683Z",
                       |  "status": "Ok"
                       |}""".stripMargin)
      implicit val restClient: RestClient.Default[IO] = new RestClient.Default[IO](backend)
      implicit val api: Api[IO]                       = new ApiImpl[IO](ApiConfig(endpoint = "http://test.local"))
      val repository                                  = new GoodsRepositoryImpl[IO]

      val expects = BuyResponse(BuyResponseStatus.Success)

      val assertation = for {
        response <- repository.buy(Token("token1"), BuyRequest(List(Domain.Id("61"), Domain.Id("1"))))
      } yield assert(response === expects)

      assertation.unsafeRunSync()
    }
  }
}
