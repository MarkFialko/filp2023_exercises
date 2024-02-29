package exercises03

object SetFunctions {
  type Set[A] = A => Boolean

  def contains[A](s: Set[A], elem: A): Boolean = {
    s(elem)
  }

  def singletonSet[A](elem: A): Set[A] = {
    Set(elem)
  }

  def union[A](s: Set[A], t: Set[A]): Set[A] = { (x: A) =>
    s(x) || t(x)
  }

  def intersect[A](s: Set[A], t: Set[A]): Set[A] = { (x: A) =>
    s(x) && t(x)
  }

  def diff[A](s: Set[A], t: Set[A]): Set[A] = { (x: A) =>
    s(x) && !t(x)
  }

  def symmetricDiff[A](s: Set[A], t: Set[A]): Set[A] = {
    union(diff(s, t), diff(t, s))
  }

  def filter[A](s: Set[A], p: A => Boolean): Set[A] = { (x: A) =>
    { s(x) && p(x) }
  }

  def cartesianProduct[A, B](as: Set[A], bs: Set[B]): Set[(A, B)] = { (x: (A, B)) =>
    {
      as(x._1) && bs(x._2)
    }
  }
}
