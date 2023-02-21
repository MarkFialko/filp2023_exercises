package exercises01

class Vector(val x: Double, val y: Double) {
  def +(other: Vector): Vector = new Vector(x + other.x, y + other.y)

  def -(other: Vector): Vector = new Vector(x - other.x, y - other.y)

  def *(scalar: Double): Vector = new Vector(x * scalar, y * scalar)

  def unary_- : Vector = new Vector(-x, -y)

  def euclideanLength: Double = Math.sqrt(x * x + y * y)

  def normalized: Vector =
    if (x == 0 && y == 0) new Vector(0, 0) else new Vector(x / euclideanLength, y / euclideanLength)
  override def equals(other: Any): Boolean = {
    other match {
      case other: Vector => {
        other.x == this.x &&
        other.y == this.y
      }
      case _ => false
    }
  }

  // Vector(x, y)
  override def toString: String = "Vector(" + x + ", " + y + ")"
}

object Vector {
  def fromAngle(angle: Double, length: Double): Vector = new Vector(length * Math.cos(angle), length * Math.sin(angle))

  def sum(list: List[Vector]): Vector =
    list.foldRight[Vector](new Vector(0, 0))((A, B) => new Vector(A.x + B.x, A.y + B.y))

  def unapply(arg: Vector): Option[(Double, Double)] = Option(arg.x, arg.y)
}
