package exercises03

import scala.annotation.tailrec

sealed trait MyList[+A]

final case class Cons[A](head: A, tail: MyList[A]) extends MyList[A]

case object Nil extends MyList[Nothing]

object MyList {
  def sum(list: MyList[Int]): Int = {
    @tailrec
    def sumHelper(list: MyList[Int], acc: Int): Int = {
      list match {
        case Cons(head, tail) => sumHelper(tail, head + acc)
        case Nil              => acc
      }
    }
    sumHelper(list, 0)
  }

  def reverse[A](list: MyList[A]): MyList[A] = {
    @tailrec
    def reverseHelper(value: MyList[A], acc: MyList[A]): MyList[A] = {
      value match {
        case Cons(head, tail) => reverseHelper(tail, Cons(head, acc))
        case Nil              => acc
      }
    }
    reverseHelper(list, Nil)
  }
}
