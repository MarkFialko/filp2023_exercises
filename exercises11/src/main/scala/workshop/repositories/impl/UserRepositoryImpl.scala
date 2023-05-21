package workshop.repositories.impl

import cats.Functor
import workshop.api.Api
import workshop.domain.Domain._
import workshop.repositories.UserRepository

class UserRepositoryImpl[F[_]: Functor: Api] extends UserRepository[F] {
  def createUser(createUser: CreateUser): F[Token] = ???

  def getUser(token: Token): F[User] = ???
}
