package workshop.repositories.impl

import cats.Functor
import io.circe.generic.JsonCodec
import workshop.api.Api
import workshop.domain.Domain._
import workshop.repositories.UserRepository
import workshop.repositories.impl.UserRepositoryImpl.TokenResponse

import cats.syntax.all._

class UserRepositoryImpl[F[_]: Functor: Api] extends UserRepository[F] {
  def createUser(user: CreateUser): F[Token] =
    Api[F].post[CreateUser, TokenResponse](_.addPath("auth", "login"), user, None).map(_.accessToken)

  def getUser(token: Token): F[User] = Api[F].get[User](_.addPath("auth", "me"), token.some)
}

object UserRepositoryImpl {
  import workshop.newtype.Instances._
  @JsonCodec(decodeOnly = true)
  case class TokenResponse(accessToken: Token)
}
