package workshop.api

import io.circe.{Decoder, Encoder}
import sttp.model.Uri
import workshop.domain.Domain.Token

trait Api[F[_]] {
  def get[Out: Decoder](apiMethod: Uri => Uri, token: Option[Token]): F[Out]

  def post[In: Encoder, Out: Decoder](apiMethod: Uri => Uri, request: In, token: Option[Token]): F[Out]
}

object Api {
  @inline
  def apply[F[_]](implicit inst: Api[F]): Api[F] =
    inst
}
