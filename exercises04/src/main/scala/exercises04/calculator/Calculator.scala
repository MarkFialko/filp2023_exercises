package exercises04.calculator

import scala.Integral.Implicits.infixIntegralOps

// Необходимо реализовать функцию сalculate для вычисления выражений
class Calculator[T: Integral] {
  def isZero(t: T): Boolean =
    t == implicitly[Integral[T]].zero

  def calculate(expr: Expr[T]): Result[T] =
    expr match {
      case Mul(left, right)           => doOperation(calculate(left), calculate(right), (x, y) => x * y)
      case Plus(left, right)          => doOperation(calculate(left), calculate(right), (x, y) => x + y)
      case Minus(left, right)         => doOperation(calculate(left), calculate(right), (x, y) => x - y)
      case Div(left, right)           => div(calculate(left), calculate(right))
      case If(iff, cond, left, right) => ifOperation(calculate(cond), iff, left, right)
      case Val(x)                     => Success(x)
    }

  def doOperation(left: Result[T], right: Result[T], operation: (T, T) => T): Result[T] =
    (left, right) match {
      case (Success(x), Success(y)) => Success(operation(x, y))
      case _                        => DivisionByZero
    }

  def div(left: Result[T], right: Result[T]): Result[T] = {
    (left, right) match {
      case (Success(x), Success(y)) if !isZero(y) => Success(x / y)
      case _                                      => DivisionByZero
    }
  }

  def ifOperation(result: Result[T], iffCondition: T => Boolean, left: Expr[T], right: Expr[T]): Result[T] =
    result match {
      case Success(x) =>
        if (iffCondition(x)) {
          calculate(left)
        } else {
          calculate(right)
        }
      case _ => DivisionByZero
    }

}
