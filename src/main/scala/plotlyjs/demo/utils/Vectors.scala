package plotlyjs.demo.utils

import scala.math._
import plotlyjs.demo.utils.Matrices._

object Vectors {

  type Vector = Seq[Double]

  //case class Norm(norm: Double)
  //def ||: Double =

  implicit class ImplicitVector(thisVector: Vector) {

    def replace(i: Int, c: Double): Vector = thisVector.zipWithIndex map { case (cv, iv) => if (iv == i) c else cv }

    def norm(p: Int): Double = pow(thisVector.map(abs).map(pow(_, p)).sum, 1.0/p)
    def norm: Double = norm(2)

    def scale(s: Double): Vector = thisVector.map(_ * s)
    def negate: Vector = scale(-1)

    def normalize(p: Int): Vector = {
      val vNorm = norm(p)
      if(vNorm != 0) scale(1/vNorm) else thisVector
    }
    def normalize: Vector = normalize(2)

    def toLength(newLength: Double): Vector = normalize scale newLength

    def +(otherVector: Vector): Vector = thisVector zip otherVector map { case (c1, c2) => c1 + c2 }
    def -(otherVector: Vector): Vector = thisVector zip otherVector map { case (c1, c2) => c1 - c2 }
    def mul(otherVector: Vector): Vector = thisVector zip otherVector map { case (c1, c2) => c1 * c2 }

    def dot(otherVector: Vector): Double = mul(otherVector) reduce[Double] { case (c1, c2) => c1 + c2 }
    def angle(otherVector: Vector): Double = acos(dot(otherVector) / (norm * otherVector.norm))

    def toRowMatrix: Matrix = Seq(thisVector)
    def toColumnMatrix: Matrix = Seq(thisVector).transpose

  }

  @deprecated("Implicit class should be used.")
  def replace(v: Vector, i: Int, c: Double): Vector = v.zipWithIndex map { case (cv, iv) => if (iv == i) c else cv }

  @deprecated("Implicit class should be used.")
  def norm(p: Int, v: Vector): Double = pow(v.map(abs).map(pow(_, p)).sum, 1.0/p)
  @deprecated("Implicit class should be used.")
  def length(v: Vector): Double = norm(2, v)

  @deprecated("Implicit class should be used.")
  def scale(v: Vector, s: Double): Vector = v.map(_ * s)
  @deprecated("Implicit class should be used.")
  def negate(v: Vector): Vector = scale(v, -1)

  @deprecated("Implicit class should be used.")
  def normalize(p: Int, v: Vector): Vector = {
    val vNorm = norm(p, v)
    if(vNorm != 0) scale(v, 1/vNorm) else v
  }
  @deprecated("Implicit class should be used.")
  def normalize(v: Vector): Vector = normalize(2, v)

  @deprecated("Implicit class should be used.")
  def toLength(v: Vector, length: Double): Vector = scale(normalize(v), length)

  @deprecated("Implicit class should be used.")
  def add(v1: Vector, v2: Vector): Vector = v1 zip v2 map { case (c1, c2) => c1 + c2 }
  @deprecated("Implicit class should be used.")
  def sub(v1: Vector, v2: Vector): Vector = v1 zip v2 map { case (c1, c2) => c1 - c2 }
  @deprecated("Implicit class should be used.")
  def mul(v1: Vector, v2: Vector): Vector = v1 zip v2 map { case (c1, c2) => c1 * c2 }

  @deprecated("Implicit class should be used.")
  def dotProduct(v1: Vector, v2: Vector): Double = mul(v1, v2) reduce[Double] { case (c1, c2) => c1 + c2 }
  @deprecated("Implicit class should be used.")
  def angle(v1: Vector, v2: Vector): Double = acos(dotProduct(v1, v2) / (length(v1) * length(v2)))

  /*
  def projection(v1: Vector, v2: Vector): Vector = mul(v2, dotProduct(v1, v2))
  def orthogonal(v1: Vector, v2: Vector): Vector = sub(v1, projection(v1, v2))
  */
}
