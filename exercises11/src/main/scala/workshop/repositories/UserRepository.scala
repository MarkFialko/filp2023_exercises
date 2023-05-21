package workshop.repositories

import workshop.domain.Domain._

trait UserRepository[F[_]] {
  def createUser(createUser: CreateUser): F[Token]

  def getUser(token: Token): F[User]
}
