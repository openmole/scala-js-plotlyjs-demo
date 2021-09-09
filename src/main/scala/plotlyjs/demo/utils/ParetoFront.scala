package plotlyjs.demo.utils

import plotlyjs.demo.utils.graph.directed
import plotlyjs.demo.utils.vector.Vectors._

object ParetoFront {

  def random(dimension: Int, size: Int): Seq[Vector] = {
    var front = Seq[Vector]()

    def compareToFront(v: Vector): Double = ParetoFront.compareToFront(front, v)

    def stepTowardFront(s: Double, v: Vector): Vector = {
      v + front.flatMap(_.zip(v).map({ case (_c, c) => _c - c })).filter(math.signum(_) == s).minBy(math.abs)
    }

    def frontBounds(v: Vector): (Vector, Vector) = {
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

    front = front ++ (0 until dimension).map((0 at dimension).replace(_, 1))
    for(_ <- 1 to size) {
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
    (v1 - v2).map(math.signum).filterNot(_ == 0).reduceOption((s1, s2) => if(s1 == s2) s1 else 0).getOrElse(0)
  }

  def compareToFront(front: Seq[Vector], v: Vector): Double = {
    front.map(compare(v, _)).filterNot(_ == 0).headOption.getOrElse(0)
  }

  def graph(front: Seq[Vector]): directed.Graph[Vector] = {
    directed.Graph.from(front.flatMap(v0 => {
      front.zipWithIndex
        .filterNot(_._1 == v0)
        .map { case (v, i) => (v - v0, i) }
        .filter(_._1.count(_ > 0) == 1)
        .groupBy(_._1.indexWhere(_ > 0))
        .map { case (index, group) =>
          group
            .minBy { case (v, _) => math.abs(v(index)) }
            ._2
        }
        .map(i => directed.Graph.ImplicitTail(front(i)) --> v0)
    }).toSet)
  }

  def oneObjectiveCompromiseGraph(front: Seq[Vector], v0: Vector): directed.weighted.Graph[Vector, Int] = {
    directed.weighted.Graph.from(front.zipWithIndex
      .filterNot(_._1 == v0)
      .map { case (v, i) => (v - v0, i) }
      .filter(_._1.count(_ > 0) == 1)
      .groupBy(_._1.indexWhere(_ > 0))
      .map { case (index, group) => (index, group.minBy { case (v, _) => math.abs(v(index)) }._2) }
      .map { case (dimensionIndex, vectorIndex) => directed.weighted.Graph.ImplicitTail(v0) --dimensionIndex-> front(vectorIndex) }
      .toSet
    )
  }

  def compromise(front: Seq[Vector], v0: Vector): Seq[(Vector, Int)] = {
    front.zipWithIndex
      .map { case (v, _) => (v, (v - v0).count(_ > 0))}
  }

  def strength(front: Seq[Vector]): Seq[Vector] = {
    front.map(v => {
      front
        .filterNot(_ == v)
        .map(_.sub(v))
        .transpose
        .map(_.filter(_ > 0).minOption.getOrElse(Double.PositiveInfinity))
    })
  }

}
