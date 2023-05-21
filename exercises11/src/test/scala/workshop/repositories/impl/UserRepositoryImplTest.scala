package workshop.repositories.impl

import cats.effect.{ContextShift, IO}
import doobie.util.ExecutionContexts
import io.odin.Logger
import org.scalatest.wordspec.AnyWordSpec
import sttp.client3.asynchttpclient.cats.AsyncHttpClientCatsBackend
import sttp.model.{HeaderNames, MediaType, Method, Uri}
import workshop.api.Api
import workshop.api.impl.ApiImpl
import workshop.config.ApiConfig
import workshop.domain.Domain._
import workshop.http.client.RestClient
import cats.syntax.option._
import org.scalatest.matchers.should.Matchers._
import sttp.client3.StringBody

class UserRepositoryImplTest extends AnyWordSpec {
  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContexts.synchronous)
  implicit val logger: Logger[IO]   = Logger.noop[IO]

  "UserRepository" should {
    "create user" in {
      val backend = AsyncHttpClientCatsBackend
        .stub[IO]
        .whenRequestMatches(request =>
          request.method === Method.POST &&
            request.uri === Uri.unsafeParse("http://test.local/auth/login") &&
            request.body === StringBody(
              """{"name":"name1","surname":"surname1","group":"group1"}""",
              "utf-8",
              MediaType.TextPlain
            )
        )
        .thenRespond("""{
                       |  "payload": {
                       |    "accessToken": "token1",
                       |    "expiresIn": 1234
                       |  },
                       |  "responseTime": 143,
                       |  "time": "2021-05-19T11:28:42.683Z",
                       |  "status": "Ok"
                       |}""".stripMargin)
      implicit val restClient: RestClient.Default[IO] = new RestClient.Default[IO](backend)
      implicit val api: Api[IO]                       = new ApiImpl[IO](ApiConfig(endpoint = "http://test.local"))
      val repository                                  = new UserRepositoryImpl[IO]

      val assertation = for {
        token <- repository.createUser(CreateUser("name1", "surname1", "group1"))
      } yield assert(token === Token("token1"))

      assertation.unsafeRunSync()
    }
  }

  "UserRepository" should {
    "read user" in {
      val backend = AsyncHttpClientCatsBackend
        .stub[IO]
        .whenRequestMatches(request =>
          request.method === Method.GET &&
            request.uri === Uri.unsafeParse("http://test.local/auth/me") &&
            request.header(HeaderNames.Authorization) === "Bearer token1".some
        )
        .thenRespond("""
                       |{
                       |  "payload":{
                       |    "id": "3",
                       |    "name": "name1",
                       |    "surname": "surname1",
                       |    "group": "group1",
                       |    "balance": "123.4"
                       |  },
                       |  "responseTime": 143,
                       |  "time": "2021-05-19T11:28:42.683Z",
                       |  "status": "Ok"
                       |}""".stripMargin)
      implicit val restClient: RestClient.Default[IO] = new RestClient.Default[IO](backend)
      implicit val api: Api[IO]                       = new ApiImpl[IO](ApiConfig(endpoint = "http://test.local"))
      val repository                                  = new UserRepositoryImpl[IO]

      val expects = User(Id("3"), "name1", "surname1", "group1", 123.4)

      val assertation = for {
        user <- repository.getUser(Token("token1"))
      } yield assert(user === expects)

      assertation.unsafeRunSync()
    }
  }

}
