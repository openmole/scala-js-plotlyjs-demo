package plotlyjs.demo.utils

import plotlyjs.demo.utils.Vectors._

import scala.math.{abs, signum}

import ParetoFront._

class ParetoFront(_dimension: Int) {

  val dimension: Int = _dimension
  private var _front = Seq[Vector]()
  def front: Seq[Vector] = _front

  private def towardFrontDirection(v: Vector) = {
    -compare(this, v)
  }

  private def stepTowardFront(s: Double)(v: Vector): Vector = {
    v + _front.flatMap(_.zip(v).map({ case (_c, c) => _c - c })).filter(signum(_) == s).minBy(abs)
  }

}

object ParetoFront {

  def compare(v1: Vector, v2: Vector): Double = {
    (v1 - v2).map(signum).filterNot(_ == 0).reduceOption((s1, s2) => if(s1 == s2) s1 else 0).getOrElse(0)
  }

  def compare(paretoFront: ParetoFront, v: Vector): Double = {
    paretoFront._front.map(compare(v, _)).filterNot(_ == 0).headOption.getOrElse(0)
  }

}
