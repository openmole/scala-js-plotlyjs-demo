package plotlyjs.demo.help

import plotlyjs.demo.utils.Vectors._

import scala.math._

object PPSE {

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
