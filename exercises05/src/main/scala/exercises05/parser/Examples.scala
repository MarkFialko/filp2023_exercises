package exercises05.parser

import exercises05.either.EitherCombinators._
import Error._

object Examples {

  /**
   * если rawUser.firstName или rawUser.secondName == None, то функция должна вернуть None
   * если rawUser.passport == None или rawUser.thirdName == None, то и в результирующем User эти поля == None
   * passport должен быть передан в формате 1234 567890, если не так, то функция должна вернуть None
   * если rawUser.id не парсится в Long то функция должна вернуть None
   * если rawUser.banned, то вернуть None
   * используйте for-comprehension
   */
  def transformToOption(rawUser: RawUser): Option[User] =
    for (firstName  <- rawUser.firstName if rawUser.firstName.isDefined;
         secondName <- rawUser.secondName if rawUser.secondName.isDefined;
         id         <- rawUser.id.toLongOption;
         passport   <- checkPassport(rawUser.passport)
         if !rawUser.banned) yield User(id, UserName(firstName, secondName, rawUser.thirdName), passport)

  private def checkPassport(passport: Option[String]): Option[Option[Passport]] = passport match {
    case Some(value) =>
      val res = value.split(" ")
      res.length match {
        case 2 =>
          (res(0).toLongOption, res(1).toLongOption) match {
            case (Some(value1), Some(value2)) if value1 < 10000 && value2 < 1000000 =>
              Some(Some(Passport(value1, value2)))
            case _ => None
          }
        case _ => None
      }
    case None => Some(None)
  }

  /**
   * если rawUser.firstName или rawUser.secondName == None, то функция должна вернуть Left(InvalidName)
   * если rawUser.passport == None или rawUser.thirdName == None, то и в результирующем User эти поля == None
   * passport должен быть передан в формате 1234 567890, если не так, то функция должна вернуть Left(InvalidPassport)
   * если rawUser.id не парсится в Long то функция должна вернуть Left(InvalidId)
   * если rawUser.banned, то вернуть Left(Banned)
   * у ошибок есть приоритет:
   * 1. Banned
   * 2. InvalidId
   * 3. InvalidName
   * 4. InvalidPassport
   * используйте for-comprehension
   * но для того, чтобы for-comprehension заработал надо реализовать map и flatMap в Either
   */
  def transformToEither(rawUser: RawUser): Either[Error, User] =
    for (_          <- if (rawUser.banned) Left(Banned) else Right();
         id         <- Either.fromOption(rawUser.id.toLongOption)(InvalidId);
         firstName  <- Either.fromOption(rawUser.firstName)(InvalidName);
         secondName <- Either.fromOption(rawUser.secondName)(InvalidName);
         passport   <- Either.fromOption(checkPassport(rawUser.passport))(InvalidPassport))
    yield User(id, UserName(firstName, secondName, rawUser.thirdName), passport)
}
