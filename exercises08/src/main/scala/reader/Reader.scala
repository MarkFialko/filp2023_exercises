package reader

import typeclasses.Monad

case class Reader[R, A](run: R => A)

object Reader {

  implicit def monad[R]: Monad[Reader[R, *]] = new Monad[Reader[R, *]] {

    override def flatMap[A, B](fa: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      Reader(ctx => f(fa.run(ctx)).run(ctx))

    override def pure[A](a: A): Reader[R, A] = Reader(_ => a)

    override def map[A, B](fa: Reader[R, A])(f: A => B): Reader[R, B] = flatMap(fa)(a => pure(f(a)))

  }

  def ask[R]: Reader[R, R] =
    Reader(identity)

  def ask[R, A](f: R => A): Reader[R, A] =
    Reader(f)

  def sequence[R, A](list: List[Reader[R, A]]): Reader[R, List[A]] = Reader { r =>
    list.map(_.run(r))

  }

}
