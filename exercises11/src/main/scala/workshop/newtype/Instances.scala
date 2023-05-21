package workshop.newtype

import io.circe.{Decoder, Encoder}
import io.estatico.newtype.Coercible
import shapeless.LowPriority

object Instances {
  implicit def NewTypeEncoder[R, N](
      implicit
      LP: LowPriority,
      CC: Coercible[Encoder[R], Encoder[N]],
      R: Encoder[R]
  ): Encoder[N] = CC(R)

  implicit def NewTypeDecoder[R, N](
      implicit
      LP: LowPriority,
      CC: Coercible[Decoder[R], Decoder[N]],
      R: Decoder[R]
  ): Decoder[N] = CC(R)
}
