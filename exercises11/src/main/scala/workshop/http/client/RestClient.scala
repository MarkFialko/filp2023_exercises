package workshop.http.client

import cats.effect.{Concurrent, ContextShift, Sync}
import cats.syntax.all._
import io.circe.syntax._
import io.circe.{Decoder, Encoder}
import io.odin.Logger
import sttp.model.{StatusCode, Uri}
import workshop.http.client.Errors._

trait RestClient[F[_]] {
  def get[Out: Decoder](uri: Uri, headers: Map[String, String] = Map.empty): F[Out]

  def post[In: Encoder, Out: Decoder](uri: Uri, body: In, headers: Map[String, String] = Map.empty): F[Out]
}

object RestClient {
  @inline
  def apply[F[_]](implicit inst: RestClient[F]): RestClient[F] =
    inst

  import sttp.client3._
  import sttp.client3.asynchttpclient.cats.AsyncHttpClientCatsBackend
  import sttp.model.MediaType

  @SuppressWarnings(Array("Disable.Any"))
  class Default[F[_]: Sync: Logger](backend: SttpBackend[F, Any]) extends RestClient[F] {
    def get[Out: Decoder](uri: Uri, headers: Map[String, String] = Map.empty): F[Out] =
      for {
        _        <- Logger[F].info(s"Sending HTTP Request GET $uri Headers: $headers")
        response <- backend.send(basicRequest.get(uri).headers(headers))
        _        <- logResponse(response)
        _        <- validateStatus(response.code)
        out      <- parse[Out](response.body)
      } yield out

    def post[In: Encoder, Out: Decoder](uri: Uri, body: In, headers: Map[String, String] = Map.empty): F[Out] = ???

    private def parse[Out: Decoder](body: Either[String, String]): F[Out] =
      body match {
        case Left(err) => Sync[F].raiseError(InvalidResponseBody(err))
        case Right(value) =>
          io.circe.parser.parse(value).flatMap(Decoder[Out].decodeJson) match {
            case Left(err)    => Sync[F].raiseError(InvalidResponseBody(err.getMessage))
            case Right(value) => Sync[F].pure(value)
          }
      }

    private def validateStatus(code: StatusCode): F[Unit] =
      if (code.isSuccess)
        Sync[F].unit
      else
        Sync[F].raiseError(InvalidStatusCode(code.code))

    private def logResponse(response: Response[Either[String, String]]): F[Unit] =
      Logger[F].info(s"Received HTTP Response ${response.code.code} ${response.body}")
  }

  object Default {
    def apply[F[_]: Sync: Concurrent: ContextShift: Logger]: F[RestClient[F]] =
      for {
        backend <- AsyncHttpClientCatsBackend.apply[F]()
      } yield new Default(backend)
  }
}
