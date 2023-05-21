package workshop.api

import enumeratum._
import io.circe.Decoder
import io.circe.generic.JsonCodec

object Domain {
  sealed trait Status extends EnumEntry
  object Status extends Enum[Status] with CirceEnum[Status] {
    case object Ok    extends Status
    case object Error extends Status

    def values: IndexedSeq[Status] = findValues
  }

  sealed trait ApiResponse[+A]
  object ApiResponse {
    @JsonCodec(decodeOnly = true)
    case class Success[A](payload: A) extends ApiResponse[A]

    @JsonCodec(decodeOnly = true)
    case class Error(payload: ErrorPayload) extends ApiResponse[Nothing]

    implicit def responseDecoder[A: Decoder]: Decoder[ApiResponse[A]] = ???
  }

  @JsonCodec(decodeOnly = true)
  case class ErrorPayload(message: String)

  case class ApiError(message: String) extends Exception(s"API Error: $message")
}
