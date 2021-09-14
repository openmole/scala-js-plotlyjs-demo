package plotlyjs.demo.homemade.utils

import scala.math.{abs, acos, pow}

object Vectors {

  type Vector = Seq[Double]

  def pNorm(v: Vector, p: Int): Double = pow(v.map(abs).map(pow(_, p)).sum, 1.0 / p)

  def basis(dimension: Int): Seq[Vector] = {
    val origin = 0 at dimension
    (0 until dimension).map(origin.replace(_, 1))
  }

  implicit class ImplicitVector(v: Vector) {

    def dimension: Int = v.length

    def replace(i: Int, c: Double): Vector = v.zipWithIndex map { case (cv, iv) => if (iv == i) c else cv }

    def replace(i: Int, f: Double => Double): Vector = replace(i, f(v(i)))

    def insert(i: Int, c: Double): Vector = v.take(i) ++ Seq(c) ++ v.drop(i)

    def remove(i: Int): Vector = v.zipWithIndex.filterNot(_._2 == i).map(_._1)

    def scale(s: Double): Vector = v.map(_ * s)

    def negate: Vector = scale(-1)

    def pNorm(p: Int): Double = Vectors.pNorm(v, p)

    def norm: Double = pNorm(2)

    def toNorm(norm: Vector => Double, value: Double): Vector = {
      val vNorm = norm(v)
      if (vNorm != 0) v.scale(value / vNorm) else v
    }

    def toNorm(value: Double): Vector = toNorm(Vectors.pNorm(_, 2), value)

    def normalize(norm: Vector => Double): Vector = v.toNorm(norm, 1)

    def normalize: Vector = v.normalize(Vectors.pNorm(_, 2))

    def add(ov: Vector): Vector = v zip ov map { case (c, oc) => c + oc }

    def add(c: Double): Vector = v.map(_ + c)

    def sub(ov: Vector): Vector = v zip ov map { case (c, oc) => c - oc }

    def sub(c: Double): Vector = v.map(_ - c)

    def mul(ov: Vector): Vector = v zip ov map { case (c, oc) => c * oc }

    def div(ov: Vector): Vector = v zip ov map { case (c, oc) => c / oc }

    def distance(norm: Vector => Double, ov: Vector): Double = norm(v.sub(ov))

    def distance(ov: Vector): Double = distance(Vectors.pNorm(_, 2), ov)

    def dot(ov: Vector): Double = v.mul(ov).sum

    def angle(ov: Vector): Double = acos(v.dot(ov) / (v.norm * ov.norm))

    def parallelComponent(ov: Vector): Vector = {
      val u = ov.normalize
      u.scale(v.dot(u))
    }

    def orthogonalComponent(ov: Vector): Vector = v - v.parallelComponent(ov)

    def *(s: Double): Vector = scale(s)

    def /(d: Double): Vector = scale(1 / d)

    def /(ov: Vector): Vector = div(ov)

    def +(ov: Vector): Vector = add(ov)

    def +(c: Double): Vector = add(c)

    def -(ov: Vector): Vector = sub(ov)

    def -(c: Double): Vector = sub(c)

    def ^(ov: Vector): Double = angle(ov)

    def vectorToString: String = "(" + v.mkString(", ") + ")"
  }

  implicit class ImplicitScalar(d: Double) {
    def *(v: Vector): Vector = v.scale(d)

    def +(v: Vector): Vector = v.add(d)

    def -(v: Vector): Vector = v.negate.add(d)

    def at(dimension: Int): Vector = Seq.fill(dimension)(d)
  }

  implicit class ImplicitScalarSupplier(f: () => Double) {
    def at(dimension: Int): Vector = Seq.fill(dimension)(f())
  }

}
