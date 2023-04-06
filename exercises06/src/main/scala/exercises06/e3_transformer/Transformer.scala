package exercises06.e3_transformer

import exercises06.e3_transformer.Error.{InvalidId, InvalidName}

trait Transformer[A, B] {
  def toOption(a: A): Option[B]

  def toEither(a: A): Either[Error, B]
}

object TransformerInstances {
  implicit val transformer: Transformer[RawUser, User] = new Transformer[RawUser, User] {
    override def toEither(rawUser: RawUser): Either[Error, User] =
      for {
        id         <- rawUser.id.toLongOption.toRight(InvalidId)
        firstName  <- rawUser.firstName.toRight(InvalidName)
        secondName <- rawUser.secondName.toRight(InvalidName)
      } yield User(id, UserName(firstName, secondName, rawUser.thirdName))

    override def toOption(rawUser: RawUser): Option[User] =
      for {
        id         <- rawUser.id.toLongOption
        firstName  <- rawUser.firstName
        secondName <- rawUser.secondName
      } yield User(id, UserName(firstName, secondName, rawUser.thirdName))
  }
}

object TransformerSyntax {
  implicit class TransformerOps[A](rawUser: A) {
    def transformToOption[B](implicit eq: Transformer[A, B]): Option[B]        = eq.toOption(rawUser)
    def transformToEither[B](implicit eq: Transformer[A, B]): Either[Error, B] = eq.toEither(rawUser)
  }
}

object Examples {
  import TransformerInstances._
  import TransformerSyntax._

  RawUser("1234", Some(""), Some(""), None).transformToOption[User]
  RawUser("1234", Some(""), Some(""), None).transformToEither[User]
}
