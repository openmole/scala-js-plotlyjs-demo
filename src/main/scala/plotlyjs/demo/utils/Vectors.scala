package plotlyjs.demo.utils

import scala.math._
import plotlyjs.demo.utils.Matrices._
import plotlyjs.demo.utils.Vectors.{norm, normalize, replace, scale, toNorm}

object Vectors {

  type Vector = Seq[Double]

  //Definitions
  def dimension(v: Vector): Int = v.length
  def toRowMatrix(v: Vector): Matrix = Seq(v)
  def toColumnMatrix(v: Vector): Matrix = toRowMatrix(v).transpose

  def replace(v: Vector, i: Int, c: Double): Vector = v.zipWithIndex map { case (cv, iv) => if (iv == i) c else cv }
  def norm(v: Vector, p: Int): Double = pow(v.map(abs).map(pow(_, p)).sum, 1.0/p)
  def scale(v: Vector, s: Double): Vector = v.map(_ * s)
  def normalize(v: Vector, p: Int): Vector = {
    val vNorm = norm(v, p)
    if(vNorm != 0) scale(v, 1/vNorm) else v
  }
  def toNorm(v: Vector, p: Int, d: Double): Vector = scale(normalize(v, p), d)
  def add(v1: Vector, v2: Vector): Vector = v1 zip v2 map { case (c1, c2) => c1 + c2 }
  def sub(v1: Vector, v2: Vector): Vector = v1 zip v2 map { case (c1, c2) => c1 - c2 }
  def mul(v1: Vector, v2: Vector): Vector = v1 zip v2 map { case (c1, c2) => c1 * c2 }
  def dot(v1: Vector, v2: Vector): Double = mul(v1, v2).sum
  def angle(v1: Vector, v2: Vector): Double = acos(dot(v1, v2) / (norm(v1) * norm(v2)))
  //

  //Currying
  def replace(i: Int, c: Double): Vector => Vector = (v: Vector) => replace(v, i, c)
  def norm(p: Int): Vector => Double = (v: Vector) => norm(v, p)
  def scale(s: Double): Vector => Vector = (v: Vector) => scale(v, s)
  def normalize(p: Int): Vector => Vector = (v: Vector) => normalize(v, p)
  def toNorm(p: Int, d: Double): Vector => Vector = (v: Vector) => toNorm(v, p, d)
  def add(v1: Vector): Vector => Vector = (v2: Vector) => add(v1, v2)
  def sub(v1: Vector): Vector => Vector = (v2: Vector) => sub(v1, v2)
  def mul(v1: Vector): Vector => Vector = (v2: Vector) => mul(v1, v2)
  def dot(v1: Vector): Vector => Double = (v2: Vector) => dot(v1, v2)
  def angle(v1: Vector): Vector => Double = (v2: Vector) => angle(v1, v2)
  //

  //Parameter aliases
  def norm: Vector => Double = norm(2)
  def normalize: Vector => Vector = normalize(2)
  def toNorm(d: Double): Vector => Vector = toNorm(2, d)
  def negate: Vector => Vector = scale(-1)
  //

  //Function aliases
  def - : Vector => Vector = negate
  //

  //Implicit class
  implicit class ImplicitVector(v: Vector) {

    def dimension: Int = Vectors.dimension(v)
    def toRowMatrix: Matrix = Vectors.toRowMatrix(v)
    def toColumnMatrix: Matrix = Vectors.toColumnMatrix(v)

    //Currying copy
    def replace(i: Int, c: Double): Vector = Vectors.replace(i, c)(v)
    def norm(p: Int): Double = Vectors.norm(p)(v)
    def scale(s: Double): Vector = Vectors.scale(s)(v)
    def normalize(p: Int): Vector = Vectors.normalize(p)(v)
    def toNorm(p: Int, d: Double): Vector = Vectors.toNorm(p, d)(v)
    def add(ov: Vector): Vector = Vectors.add(ov)(v)
    def sub(ov: Vector): Vector = Vectors.sub(ov)(v)
    def mul(ov: Vector): Vector = Vectors.mul(ov)(v)
    def dot(ov: Vector): Double = Vectors.dot(ov)(v)
    def angle(ov: Vector): Double = Vectors.angle(ov)(v)
    //

    //Parameter aliases copy
    def norm: Double = Vectors.norm(v)
    def normalize: Vector = Vectors.normalize(v)
    def toNorm(d: Double): Vector = Vectors.toNorm(d)(v)
    def negate: Vector = Vectors.negate(v)
    //

    //Function aliases
    def *(s: Double): Vector = scale(s)
    def +(ov: Vector): Vector = add(ov)
    def -(ov: Vector): Vector = sub(ov)
    def ^(ov: Vector): Double = angle(ov)
    //

  }
  //

  /*
  def projection(v1: Vector, v2: Vector): Vector = mul(v2, dotProduct(v1, v2))
  def orthogonal(v1: Vector, v2: Vector): Vector = sub(v1, projection(v1, v2))
  */

}
