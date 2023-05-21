package workshop.db

import doobie.util.meta.Meta
import io.estatico.newtype.BaseNewType

@SuppressWarnings(Array("DisableSyntax.asInstanceOf"))
object Instances {
  implicit def newTypeDoobieMeta[B, T, R](implicit underlying: Meta[R]): Meta[BaseNewType.Aux[B, T, R]] =
    underlying.asInstanceOf[Meta[BaseNewType.Aux[B, T, R]]]
}
