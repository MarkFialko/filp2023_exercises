package exercises06.e4_eq

trait Eq[A] {
  def eqv(a: A, b: A): Boolean
}

object Eq {}

object EqInstances {
  implicit val intEq: Eq[Int]       = (a: Int, b: Int) => a == b
  implicit val stringEq: Eq[String] = (a: String, b: String) => a == b
  implicit val boolEq: Eq[Boolean]  = (a: Boolean, b: Boolean) => a == b
  //  implicit val eqListInt: Eq[List[Int]]       = (a: List[Int], b: List[Int]) => a.equals(b)
  //  implicit val eqListBool: Eq[List[Boolean]]  = (a: List[Boolean], b: List[Boolean]) => a.equals(b)
  //  implicit val eqOptInt: Eq[Option[Int]]      = (a: Option[Int], b: Option[Int]) => a.equals(b)
  //  implicit val eqOptBool: Eq[Option[Boolean]] = (a: Option[Boolean], b: Option[Boolean]) => a.equals(b)

  implicit def eqOpt[A](implicit eq: Eq[A]): Eq[Option[A]] =
    (a: Option[A], b: Option[A]) =>
      (a, b) match {
        case (Some(elem1), Some(elem2)) => eq.eqv(elem1, elem2)
        case (None, None)               => true
        case _                          => false
      }
  implicit def listEq[A](implicit eq: Eq[A]): Eq[List[A]] = (a: List[A], b: List[A]) => a.corresponds(b)(eq.eqv)
}

object EqSyntax {
  implicit class EqOps[A](a: A) {
    def eqv(b: A)(implicit eq: Eq[A]): Boolean = eq.eqv(a, b)
    def ===(b: A)(implicit eq: Eq[A]): Boolean = eq.eqv(a, b)
    def !==(b: A)(implicit eq: Eq[A]): Boolean = !eq.eqv(a, b)
  }
}

object Examples {
  import EqInstances._
  import EqSyntax._

  1 eqv 1 // возвращает true
  1 === 2 // возвращает false
  1 !== 2 // возвращает true
  //   1 === "some-string" // не компилируется
  //   1 !== Some(2) // не компилируется
  List(true) === List(true) // возвращает true
  Option(2) eqv Option(2)
}
