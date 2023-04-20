package writer

import typeclasses._

case class Writer[Log, A](log: Log, value: A) {
  def tell(nextLog: Log)(implicit semigroup: Semigroup[Log]): Writer[Log, A] =
    Writer(semigroup.combine(log, nextLog), value)
}

object Writer {
  implicit def monad[Log: Monoid]: Monad[Writer[Log, *]] = new Monad[Writer[Log, *]] {
    override def pure[A](a: A): Writer[Log, A] = Writer(Monoid[Log].empty, a)

    override def map[A, B](fa: Writer[Log, A])(f: A => B): Writer[Log, B] = Writer(fa.log, f(fa.value))

    override def flatMap[A, B](fa: Writer[Log, A])(f: A => Writer[Log, B]): Writer[Log, B] =
      f(fa.value) match { case next => Writer(fa.tell(next.log).log, next.value) }
  }

  def tell[Log](log: Log): Writer[Log, Unit] = Writer[Log, Unit](log, ())
}
