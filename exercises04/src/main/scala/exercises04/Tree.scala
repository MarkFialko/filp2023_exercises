package exercises04

sealed trait Tree[+A]
final case class Leaf[A](value: A)                        extends Tree[A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
    t match {
      case Leaf(a)      => f(a)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

  def size[A](t: Tree[A]): Int = fold(t)(a => 1)(1 + _ + _)

  def max(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)

  def depth[A](t: Tree[A]): Int = fold(t)(a => 1)((d1, d2) => 1 + (d1 max d2))

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}
