package plotlyjs.demo.labo.help

import SquareDiagonalTransformation.Vectors._

import scala.math._

object SquareDiagonalTransformation {

  object Vectors {

    type Vector = Seq[Double]

    //Definitions
    def dimension(v: Vector): Int = v.length

    def replace(v: Vector, i: Int, c: Double): Vector = v.zipWithIndex map { case (cv, iv) => if (iv == i) c else cv }
    def insert(v: Vector, i: Int, c: Double): Vector = {
      val (left, right) = v.splitAt(i)
      left ++ Seq(c) ++ right
    }
    def remove(v: Vector, i: Int): Seq[Double] = v.zipWithIndex.filterNot(_._2 == i).map(_._1)

    def norm(v: Vector, p: Int): Double = pow(v.map(abs).map(pow(_, p)).sum, 1.0/p)
    def scale(v: Vector, s: Double): Vector = v.map(_ * s)
    def normalize(v: Vector, p: Int): Vector = {
      val vNorm = norm(v, p)
      if(vNorm != 0) scale(v, 1/vNorm) else v
    }
    def toNorm(v: Vector, p: Int, d: Double): Vector = scale(normalize(v, p), d)
    def add(v1: Vector, v2: Vector): Vector = v1 zip v2 map { case (c1, c2) => c1 + c2 }
    def add(v: Vector, c: Double): Vector = v.map(_ + c)
    def sub(v1: Vector, v2: Vector): Vector = v1 zip v2 map { case (c1, c2) => c1 - c2 }
    def sub(v: Vector, c: Double): Vector = v.map(_ - c)
    def mul(v1: Vector, v2: Vector): Vector = v1 zip v2 map { case (c1, c2) => c1 * c2 }
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
    //

    //Function aliases
    def -(v: Vector): Vector = negate(v)
    //

    //Implicit class
    implicit class ImplicitVector(v: Vector) {

      def dimension: Int = Vectors.dimension(v)

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
      //

      //Function aliases
      def *(s: Double): Vector = scale(s)
      def *:(s: Double): Vector = scale(s)
      def +(ov: Vector): Vector = add(ov)
      def -(ov: Vector): Vector = sub(ov)
      def ^(ov: Vector): Double = angle(ov)
      //

      def vectorToString: String = "(" + v.mkString(", ") + ")"
    }
    //

  }

  def proportionBetween(v1: Vector, v2: Vector, v: Vector): Double = {
    val v1v2 = v2.sub(v1)
    val v1v = v.sub(v1)
    val u = v1v2.normalize
    (v1v dot u) / v1v2.norm
  }

  def move(v1: Vector, v2: Vector, v: Vector, f: Double => Double): Vector = {
    val newNorm = f(proportionBetween(v1, v2, v)) * v2.sub(v1).norm
    v1 + v.sub(v1).toNorm(newNorm)
  }

  def t1Bounds(v: Vector): (Vector, Vector) = {
    val s = v.sum
    if(s < 1) {
      (Seq(0, s), Seq(s, 0))
    } else {
      (Seq(s - 1, 1), Seq(1, s - 1))
    }

  }

  def t2Bounds(v: Vector): (Vector, Vector) = {
    if(v(0) > v(1)) {
      val s = (1 - v(0)) + v(1)
      (Seq(1 - s, 0), Seq(1, s))
    } else {
      val s = v(0) + (1 - v(1))
      (Seq(0, 1 - s), Seq(s, 1))
    }
  }

  /*
  def asinP(p: Double): Double = {
    (asin((p - 0.5)*2)/(Pi/2) + 1)/2
  }

  def asinPN(p: Double, n: Int): Double = {
    var newP = p
    for(_ <- 1 to n) {
      newP = asinP(newP)
    }
    newP
  }
  */

  // 0 is on the diagonal and 1 is on the border
  def t1ff(p: Double): Double = {
    pow(p, 2)
  }

  def t1f(p: Double): Double = {
    var u = (p - 0.5) * 2
    if(u > 0) {
      u = t1ff(u)
    } else {
      u = -t1ff(-u)
    }
    (u + 1)/2
  }

  // 0 is on the border including (0, 0) and 1 is on the border including (1, 1)
  def t2f(p: Double): Double = {
    pow(p, 2)
  }

  // transformation
  def move(v: Vector): Vector = {
    val t1B = t1Bounds(v)
    val vt1 = move(t1B._1, t1B._2, v, t1f)
    val t2B = t2Bounds(vt1)
    val vt1t2 = move(t2B._1, t2B._2, vt1, t2f)
    vt1t2
  }

  def vectors(n: Int): Seq[Vector] = {
    val range = (0 to n).map(_.toDouble / n)
    range.flatMap(x => {
      range.map(y => {
        move(Seq(x, y))
      })
    })
  }

}
