package workshop.http

import cats.effect.Sync
import cats.syntax.all._
import enumeratum.{CirceEnum, Enum, EnumEntry}
import io.circe.generic.JsonCodec
import io.circe.generic.semiauto.deriveEncoder
import io.circe.syntax._
import io.circe.{Decoder, Encoder}
import sttp.tapir.Endpoint
import sttp.tapir.server.ServerEndpoint

package object server {
  sealed trait Status extends EnumEntry
  object Status extends Enum[Status] with CirceEnum[Status] {
    case object Ok    extends Status
    case object Error extends Status

    override def values: IndexedSeq[Status] = findValues
  }

  sealed trait Response[+A]
  object Response {
    case class Success[+A](payload: A, status: Status = Status.Ok) extends Response[A]
    @JsonCodec(encodeOnly = true)
    case class Error(error: String, status: Status = Status.Error) extends Response[Nothing]

    implicit def successDecoder[A]: Decoder[Response[A]] = _ => Right(Error("Implementation missing"))

    implicit def successEncoder[A: Encoder]: Encoder[Success[A]] = deriveEncoder

    implicit def responseEncoder[A](implicit ev: Encoder[Success[A]]): Encoder[Response[A]] = {
      case s @ Success(_, _) => s.asJson
      case e @ Error(_, _)   => e.asJson
    }
  }

  @SuppressWarnings(Array("Disable.Any"))
  implicit class EndpointReacher[I, O](endpoint: Endpoint[I, Unit, Response[O], Any]) {
    def handle[F[_]: Sync](
        logic: I => F[O]
    ): ServerEndpoint[I, Unit, Response[O], Any, F] =
      ServerEndpoint[I, Unit, Response[O], Any, F](
        endpoint,
        _ =>
          i => {
            logic(i).attempt
              .handleErrorWith(th => Sync[F].delay(Left(th)))
              .flatMap[Response[O]] {
                case Left(th)     => Sync[F].pure(Response.Error(th.getMessage))
                case Right(value) => Sync[F].pure(Response.Success(value))
              }
              .map(_.asRight[Unit])
          }
      )
  }
}
