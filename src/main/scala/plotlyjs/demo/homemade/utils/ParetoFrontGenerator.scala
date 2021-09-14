package plotlyjs.demo.homemade.utils

import plotlyjs.demo.homemade.utils.Vectors._

object ParetoFrontGenerator {

  def random(dimension: Int, size: Int): Seq[Vector] = {
    var front = Seq[Vector]()

    def compareToFront(v: Vector): Double = ParetoFrontGenerator.compareToFront(front, v)

    def stepTowardFront(s: Double, v: Vector): Vector = {
      v + front.flatMap(_.zip(v).map({ case (_c, c) => _c - c })).filter(math.signum(_) == s).minBy(math.abs)
    }

    def frontBounds(v: Vector): (Vector, Vector) = {
      val comparison = compareToFront(v)
      if (comparison == 0) {
        (stepTowardFront(-1, v), stepTowardFront(+1, v))
      } else {
        val direction = -comparison
        var vPrevious = v
        var vNext = v
        while (compareToFront((vPrevious + vNext) / 2) != 0) {
          vPrevious = vNext
          vNext = stepTowardFront(direction, vPrevious)
        }
        if (direction > 0) (vPrevious, vNext) else (vNext, vPrevious)
      }
    }

    front = front ++ (0 until dimension).map((0 at dimension).replace(_, 1))
    for (_ <- 1 to size) {
      val (v1, v2) = frontBounds((() => math.random()) at dimension)
      val epsilon = 10E-4
      val alpha = epsilon + math.random() * (1 - epsilon)
      val v = (1 - alpha) * v1 + alpha * v2
      front = front :+ v
    }
    front = front.drop(dimension)
    assert(front.map(compareToFront(_) == 0).reduce(_ && _))

    front
  }

  def compare(v1: Vector, v2: Vector): Double = {
    (v1 - v2).map(math.signum).filterNot(_ == 0).reduceOption((s1, s2) => if (s1 == s2) s1 else 0).getOrElse(0)
  }

  def compareToFront(front: Seq[Vector], v: Vector): Double = {
    front.map(compare(v, _)).filterNot(_ == 0).headOption.getOrElse(0)
  }

}
