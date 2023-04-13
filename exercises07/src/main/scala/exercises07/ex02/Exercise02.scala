package exercises07.ex02

import exercises07.data.NonEmptyList
import exercises07.ex02.Domain._

import exercises07.ex02.Errors.{InvalidAddressBookId, InvalidPersonId, InvalidPhone, MissingPersonName, ParsingError}

import exercises07.typeclasses._

object Exercise02 {

  private type TransformationSupport[F[_]] = ApplicativeError[F, NonEmptyList[ParsingError]]

  object TransformationSupport {

    def apply[F[_]](implicit inst: TransformationSupport[F]): TransformationSupport[F] =
      inst

  }

  private implicit class OptionOps[A](private val opt: Option[A]) extends AnyVal {

    def require[F[_]](err: => ParsingError)(implicit ts: TransformationSupport[F]): F[A] =
      opt match {

        case Some(value) => ts.pure(value)

        case None => ts.raiseError(NonEmptyList.of(err))

      }

  }

  // Советуем воспользоваться)

  import TupleSyntax._

  import TransformerSyntax._

  import exercises07.ex01.Exercise01.Syntax._

  import exercises07.ex01.Exercise01.Instances._

  implicit def personTransformerF[F[_]: TransformationSupport]: TransformerF[F, RawPerson, Person] =
    from =>
      (
        from.id.toLongOption.require(InvalidPersonId(from.id)),
        from.name.require(MissingPersonName),
        Phone.parse(from.phone).require(InvalidPhone(from.phone))
      ).mapN(Person)

  implicit def addressBookTransformerF[F[_]: TransformationSupport]: TransformerF[F, RawAddressBook, AddressBook] =
    from =>
      (
        from.id.toLongOption.require(InvalidAddressBookId(from.id)),
        from.persons.traverse(_.transformF)
      ).mapN(AddressBook)

}
