package plotlyjs.demo.utils

import plotlyjs.demo.utils.Matrices._

import scala.math._

object Vectors {

  type Vector = Seq[Double]

  //Definitions
  def dimension(v: Vector): Int = v.length
  def toRowMatrix(v: Vector): Matrix = Seq(v)
  def toColumnMatrix(v: Vector): Matrix = toRowMatrix(v).transpose

  def replace(v: Vector, i: Int, c: Double): Vector = v.zipWithIndex map { case (cv, iv) => if (iv == i) c else cv }
  def insert(v: Vector, i: Int, c: Double): Vector = {
    val (left, right) = v.splitAt(i)
    left ++ Seq(c) ++ right
  }
  def remove(v: Vector, i: Int): Seq[Double] = v.zipWithIndex.filterNot(_._2 == i).map(_._1)

  def norm(v: Vector, p: Int): Double = pow(v.map(abs).map(pow(_, p)).sum, 1.0/p)
  def scale(v: Vector, s: Double): Vector = v.map(_ * s)
  def normalize(v: Vector, p: Int): Vector = {//TODO normalize given a norm
    val vNorm = norm(v, p)
    if(vNorm != 0) scale(v, 1/vNorm) else v
  }
  def toNorm(v: Vector, p: Int, d: Double): Vector = scale(normalize(v, p), d)
  def add(v1: Vector, v2: Vector): Vector = v1 zip v2 map { case (c1, c2) => c1 + c2 }
  def add(v: Vector, c: Double): Vector = v.map(_ + c)
  def sub(v1: Vector, v2: Vector): Vector = v1 zip v2 map { case (c1, c2) => c1 - c2 }
  def sub(v: Vector, c: Double): Vector = v.map(_ - c)
  def mul(v1: Vector, v2: Vector): Vector = v1 zip v2 map { case (c1, c2) => c1 * c2 }
  def distance(v1: Vector, v2: Vector, norm: Vector => Double): Double = norm(sub(v1, v2))
  def dot(v1: Vector, v2: Vector): Double = mul(v1, v2).sum
  def angle(v1: Vector, v2: Vector): Double = acos(dot(v1, v2) / (norm(v1) * norm(v2)))
  def parallelComponent(v1: Vector, v2: Vector): Vector = {
    val u = normalize(v2, 2)
    scale(u, dot(v1, u))
  }
  def orthogonalComponent(v1: Vector, v2: Vector): Vector = sub(v1, parallelComponent(v1, v2))
  //

  //Currying
  def replace(i: Int, c: Double)(v: Vector): Vector = replace(v, i, c)
  def insert(i: Int, c: Double)(v: Vector): Vector = insert(v, i, c)
  def remove(i: Int)(v: Vector): Vector = remove(v, i)
  def norm(p: Int)(v: Vector): Double = norm(v, p)
  def scale(s: Double)(v: Vector): Vector = scale(v, s)
  def normalize(p: Int)(v: Vector): Vector = normalize(v, p)
  def toNorm(p: Int, d: Double)(v: Vector): Vector = toNorm(v, p, d)
  def add(v2: Vector): Vector => Vector = (v1: Vector) => add(v1, v2)
  def add(c: Double)(v: Vector): Vector = add(v, c)
  def sub(v2: Vector): Vector => Vector = (v1: Vector) => sub(v1, v2)
  def sub(c: Double)(v: Vector): Vector = sub(v, c)
  def mul(v2: Vector): Vector => Vector = (v1: Vector) => mul(v1, v2)
  def distance(norm: Vector => Double)(v2: Vector)(v1: Vector): Double = distance(v1, v2, norm)
  def dot(v2: Vector): Vector => Double = (v1: Vector) => dot(v1, v2)
  def angle(v2: Vector): Vector => Double = (v1: Vector) => angle(v1, v2)
  def parallelComponent(v2: Vector): Vector => Vector = (v1: Vector) => parallelComponent(v1, v2)
  def orthogonalComponent(v2: Vector): Vector => Vector = (v1: Vector) => orthogonalComponent(v1, v2)
  //

  //Parameter aliases
  def norm(v: Vector): Double = norm(2)(v)
  def normalize(v: Vector): Vector = normalize(2)(v)
  def toNorm(d: Double)(v: Vector): Vector = toNorm(2, d)(v)
  def negate(v: Vector): Vector = scale(-1)(v)
  def distance(v2: Vector)(v1: Vector): Double = distance(norm(_))(v2)(v1)
  //

  //Function aliases
  def -(v: Vector): Vector = negate(v)
  //

  //Implicit vector
  implicit class ImplicitVector(v: Vector) {

    def dimension: Int = Vectors.dimension(v)
    def toRowMatrix: Matrix = Vectors.toRowMatrix(v)
    def toColumnMatrix: Matrix = Vectors.toColumnMatrix(v)

    //Currying copy
    def replace(i: Int, c: Double): Vector = Vectors.replace(i, c)(v)
    def insert(i: Int, c: Double): Vector = Vectors.insert(i, c)(v)
    def remove(i: Int): Vector = Vectors.remove(i)(v)
    def norm(p: Int): Double = Vectors.norm(p)(v)
    def scale(s: Double): Vector = Vectors.scale(s)(v)
    def normalize(p: Int): Vector = Vectors.normalize(p)(v)
    def toNorm(p: Int, d: Double): Vector = Vectors.toNorm(p, d)(v)
    def add(ov: Vector): Vector = Vectors.add(ov)(v)
    def add(c: Double): Vector = Vectors.add(c)(v)
    def sub(ov: Vector): Vector = Vectors.sub(ov)(v)
    def sub(c: Double): Vector = Vectors.sub(c)(v)
    def mul(ov: Vector): Vector = Vectors.mul(ov)(v)
    def distance(norm: Vector => Double)(ov: Vector): Double = Vectors.distance(norm)(ov)(v)
    def dot(ov: Vector): Double = Vectors.dot(ov)(v)
    def angle(ov: Vector): Double = Vectors.angle(ov)(v)
    def parallelComponent(ov: Vector): Vector = Vectors.parallelComponent(ov)(v)
    def orthogonalComponent(ov: Vector): Vector = Vectors.orthogonalComponent(ov)(v)
    //

    //Parameter aliases copy
    def norm: Double = Vectors.norm(v)
    def normalize: Vector = Vectors.normalize(v)
    def toNorm(d: Double): Vector = Vectors.toNorm(d)(v)
    def negate: Vector = Vectors.negate(v)
    def distance(ov: Vector): Double = Vectors.distance(ov)(v)
    //

    //Function aliases
    def *(s: Double): Vector = scale(s)
    def /(d: Double): Vector = scale(1/d)
    def +(ov: Vector): Vector = add(ov)
    def +(c: Double): Vector = add(c)
    def -(ov: Vector): Vector = sub(ov)
    def -(c: Double): Vector = sub(c)
    def ^(ov: Vector): Double = angle(ov)
    //

    def vectorToString: String = "(" + v.mkString(", ") + ")"
  }
  //

  //Implicit scalar
  implicit class ImplicitScalar(d: Double) {
    def *(v: Vector): Vector = v * d
    def +(v: Vector): Vector = add(v, d)
    def -(v: Vector): Vector = d + (Vectors -v)
    def at(dimension: Int): Vector = Seq.fill(dimension)(d)
  }
  //

  //Implicit scalar supplier
  implicit class ImplicitScalarSupplier(f: () => Double) {
    def at(dimension: Int): Vector = Seq.fill(dimension)(f())
  }
  //

}
