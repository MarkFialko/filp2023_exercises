package workshop.domain

import enumeratum._
import io.circe.generic.JsonCodec
import io.estatico.newtype.NewType
import workshop.newtype.Instances._

object Domain {
  type Id = Id.Type
  object Id extends NewType.Default[String]

  type Token = Token.Type
  object Token extends NewType.Default[String]

  @JsonCodec
  case class CreateUser(name: String, surname: String, group: String)

  @JsonCodec(decodeOnly = true)
  case class User(id: Id, name: String, surname: String, group: String, balance: Double)

  @JsonCodec(decodeOnly = true)
  case class Good(id: Id, name: String, description: String, category: GoodCategory, price: Double)

  @JsonCodec(decodeOnly = true)
  case class GoodCategory(id: Id, name: String)

  @JsonCodec
  case class BuyResponse(status: BuyResponseStatus)

  @JsonCodec(encodeOnly = true)
  case class BuyRequest(goodIds: List[Id])

  sealed trait BuyResponseStatus extends EnumEntry
  object BuyResponseStatus extends Enum[BuyResponseStatus] with CirceEnum[BuyResponseStatus] {
    case object Success           extends BuyResponseStatus
    case object AlreadyCompleted  extends BuyResponseStatus
    case object IncorrectCategory extends BuyResponseStatus
    case object IncorrectGoods    extends BuyResponseStatus

    def values: IndexedSeq[BuyResponseStatus] = findValues
  }
}
