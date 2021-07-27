package plotlyjs.demo.utils

import plotlyjs.demo.utils.Vectors._

import scala.math._

class ParetoFront(_dimension: Int, _size: Int) {

  val dimension: Int = _dimension
  val size: Int = _size

  private var _front = Seq[Vector]()
  def front: Seq[Vector] = _front

  private def compareToFront(v: Vector): Double = ParetoFront.compareToFront(_front, v)

  private def stepTowardFront(s: Double, v: Vector): Vector = {
    v + _front.flatMap(_.zip(v).map({ case (_c, c) => _c - c })).filter(signum(_) == s).minBy(abs)
  }

  private def frontBounds(v: Vector): (Vector, Vector) = {
    val comparison = compareToFront(v)
    if(comparison == 0) {
      (stepTowardFront(-1, v), stepTowardFront(+1, v))
    } else {
      val direction = -comparison
      var vPrevious = v
      var vNext = v
      while(compareToFront((vPrevious + vNext)/2) != 0) {
        vPrevious = vNext
        vNext = stepTowardFront(direction, vPrevious)
      }
      if(direction > 0) (vPrevious, vNext) else (vNext, vPrevious)
    }
  }

  _front = _front ++ (0 until dimension).map((0 at dimension).replace(_, 1))
  for(_ <- 1 to _size) {
    val (v1, v2) = frontBounds((() => random) at dimension)
    val epsilon = 10E-4
    val alpha = epsilon + random * (1 - epsilon)
    val v = (1 - alpha) * v1 + alpha * v2
    _front = _front :+ v
  }
  _front = _front.drop(dimension)

  assert(_front.map(compareToFront(_) == 0).reduce(_ && _))

  def randomizeDimensions: ParetoFront = {
    _front = _front.map(mul((() => ceil(10 * random)) at dimension))
    this
  }

}

object ParetoFront {

  def compare(v1: Vector, v2: Vector): Double = {
    (v1 - v2).map(signum).filterNot(_ == 0).reduceOption((s1, s2) => if(s1 == s2) s1 else 0).getOrElse(0)
  }

  def compareToFront(front: Seq[Vector], v: Vector): Double = {
    front.map(compare(v, _)).filterNot(_ == 0).headOption.getOrElse(0)
  }

}
