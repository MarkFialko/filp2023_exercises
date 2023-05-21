package workshop.http.client

object Errors {
  case class InvalidStatusCode(code: Int) extends Exception(s"Invalid HTTP status code: $code")

  case class InvalidResponseBody(error: String) extends Exception(s"Invalid response body: $error")
}
