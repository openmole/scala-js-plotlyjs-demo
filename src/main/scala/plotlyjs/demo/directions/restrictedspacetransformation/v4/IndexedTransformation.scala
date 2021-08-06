package plotlyjs.demo.directions.restrictedspacetransformation.v4

import plotlyjs.demo.directions.restrictedspacetransformation.v4.Geometry._
import plotlyjs.demo.utils.vector.IntVectors._
import plotlyjs.demo.directions.restrictedspacetransformation.v4.Transformation._
import plotlyjs.demo.utils.vector.Vectors._

import scala.math._

object IndexedTransformation {

  def fromIndexToCircle(indexVector: IntVector): Option[Vector] = fromSquareToCircle(indexVector)
  def fromCircleToIndex(circleVector: Vector): IntVector = fromCircleToSquare(circleVector)

  def centeredNCubeSurface(dimension: Int, radius: Int): Iterable[IntVector] = {
    centeredNCube(dimension - 1, radius - 1).flatMap(indexVector => {
      (0 until dimension).flatMap(i => {
        Seq(-1.0, +1.0).map(_ * radius).map(u => {
          indexVector.vector.insert(i, u)
        })
      })
    })
  }

  def circle(dimension: Int, radius: Int): Iterable[Vector] = {
    centeredNCubeSurface(dimension, radiusFromSquareToCircle(dimension - 1)(radius).toInt + 1).flatMap(fromIndexToCircle)
  }

  def circleWithIndex(dimension: Int, radius: Int): Iterable[(Vector, IntVector)] = {
    circle(dimension, radius).map(circleVector => (circleVector, fromCircleToIndex(circleVector)))
  }

  def circleTest(dimension: Int, radius: Int): Unit = {
    val result = circle(dimension, radius)
    println(dimension, radius, result.size)
  }

  def inverseCircleTest(indexVector: IntVector): Option[Double] = {
    val circleVectorOption = fromIndexToCircle(indexVector)
    circleVectorOption.map(circleVector => {
      val recoveredIndexVector = fromCircleToIndex(circleVector)
      (recoveredIndexVector.vector - indexVector).norm
    })
  }

  def inverseCircleTest(): Unit = {
    val dimension = 3
    val radius = 8
    centeredNCubeSurface(dimension, radius).foreach(squareVector => {
      inverseCircleTest(squareVector).foreach(println)
    })
  }

  def swapMagnitudes(v: Vector, i1: Int, i2: Int): Vector = {
    v.replace(i1, signum(v(i1)) * abs(v(i2))).replace(i2, signum(v(i2)) * abs(v(i1)))
  }

  def neighbourhood(indexVector: IntVector): Seq[IntVector] = {
    val vector = indexVector.vector
    val dimension = vector.dimension
    val faceMaxMagnitude = MaxMagnitude(vector)
    val sectorMaxMagnitude = MaxMagnitude(faceMaxMagnitude.fullSpaceRemainder)
    val faceIndex = faceMaxMagnitude.index
    val sectorIndex = sectorMaxMagnitude.index
    (0 until dimension).filterNot(_ == faceIndex).flatMap(i => {
      Seq(-1.0, +1.0).flatMap(u => {
        val neighbour = vector.replace(i, _ + u)
        fromSquareToCircle(neighbour).map(_ => neighbour).orElse({
          if(i != sectorIndex) {
            Some(swapMagnitudes(vector, sectorIndex, i))
          } else if(i == sectorIndex && signum(u) == sectorMaxMagnitude.signum) {
            Some(swapMagnitudes(vector, faceIndex, sectorIndex))
          } else {
            None
          }
        })
      })
    }).map[IntVector](iv => iv)
  }

}
