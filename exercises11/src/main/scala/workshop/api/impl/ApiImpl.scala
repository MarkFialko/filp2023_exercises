package workshop.api.impl

import cats.effect.Sync
import io.circe.{Decoder, Encoder}
import sttp.model.Uri
import workshop.api.Api
import workshop.config.ApiConfig
import workshop.domain.Domain.Token
import workshop.http.client.RestClient

class ApiImpl[F[_]: Sync: RestClient](cfg: ApiConfig) extends Api[F] {
  def get[Out: Decoder](apiMethod: Uri => Uri, token: Option[Token]): F[Out] = ???
  def post[In: Encoder, Out: Decoder](apiMethod: Uri => Uri, request: In, token: Option[Token]): F[Out] = ???
}