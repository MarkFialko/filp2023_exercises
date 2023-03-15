package exercises03

object Partial {
  def combo[I, T](funcs: List[PartialFunction[I, T]]): I => Option[T] = { input: I =>
    funcs.foldLeft(Option.empty[T])((acc, f) => if (acc.isDefined) acc else f.lift(input) orElse acc)
  }
}
