package workshop.api.impl

import cats.effect.Sync
import io.circe.{Decoder, Encoder}
import sttp.model.Uri
import cats.syntax.all._
import sttp.model.Uri._
import workshop.api.Api
import workshop.api.Domain.{ApiResponse, ApiError}
import workshop.config.ApiConfig
import workshop.api.impl.ApiImpl.tokenHeaders
import workshop.domain.Domain.Token
import workshop.http.client.RestClient

class ApiImpl[F[_]: Sync: RestClient](cfg: ApiConfig) extends Api[F] {
  def get[Out: Decoder](apiMethod: Uri => Uri, token: Option[Token]): F[Out] =
    RestClient[F].get[ApiResponse[Out]](apiMethod(uri"${cfg.endpoint}"), tokenHeaders(token)).flatMap(validateResponse)
  def post[In: Encoder, Out: Decoder](apiMethod: Uri => Uri, request: In, token: Option[Token]): F[Out] =
    RestClient[F]
      .post[In, ApiResponse[Out]](apiMethod(uri"${cfg.endpoint}"), request, tokenHeaders(token))
      .flatMap(validateResponse)

  private def validateResponse[A](res: ApiResponse[A]): F[A] =
    res match {
      case ApiResponse.Success(payload) => Sync[F].pure(payload)
      case ApiResponse.Error(payload)   => Sync[F].raiseError(ApiError(payload.message))
    }
}

object ApiImpl {
  def tokenHeaders(token: Option[Token]): Map[String, String] =
    token.fold[Map[String, String]](Map.empty)(t => Map("Authorization" -> s"Bearer $t"))
}
