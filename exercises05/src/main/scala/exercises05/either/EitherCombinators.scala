package exercises05.either

object EitherCombinators {

  sealed trait Either[+A, +B] {
    def map[C](f: B => C): Either[A, C] = this match {
      case Left(get)  => Left(get)
      case Right(get) => Right(f(get))
    }

    def flatMap[X >: A, C](f: B => Either[X, C]): Either[X, C] = this match {
      case Left(get)  => Left(get)
      case Right(get) => f(get)
    }

    def orElse[EE >: A, C >: B](other: => Either[EE, C]): Either[EE, C] = this match {
      case Left(_) =>
        other match {
          case Left(_)  => this
          case Right(_) => other
        }
      case Right(_) => this
    }

    def map2[AA >: A, BB, C](other: => Either[AA, BB])(f: (B, BB) => C): Either[AA, C] = this match {
      case Left(get) => Left(get)
      case Right(get) =>
        other match {
          case Left(get1)  => Left(get1)
          case Right(get1) => Right(f(get, get1))
        }
    }
  }

  case class Left[+A, +B](get: A) extends Either[A, B]

  case class Right[+A, +B](get: B) extends Either[A, B]

  object Either {
    def fromOption[A, B](option: Option[B])(a: => A): Either[A, B] = option match {
      case Some(value) => Right(value)
      case None        => Left(a)
    }

    def traverse[E, A, B](list: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      list.foldLeft[Either[E, List[B]]](Right(List()))((acc, elem) =>
        acc match {
          case Left(get) => Left(get)
          case Right(get) =>
            f(elem) match {
              case Left(get1)  => Left(get1)
              case Right(get1) => Right(get :+ get1)
            }
        }
      )

    def sequence[E, A](list: List[Either[E, A]]): Either[E, List[A]] = traverse(list)(identity)
  }

}
