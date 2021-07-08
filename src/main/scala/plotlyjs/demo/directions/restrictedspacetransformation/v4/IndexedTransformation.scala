package plotlyjs.demo.directions.restrictedspacetransformation.v4

import plotlyjs.demo.directions.restrictedspacetransformation.v4.IndexVectors._
import plotlyjs.demo.directions.restrictedspacetransformation.v4.Transformation._
import plotlyjs.demo.utils.Vectors._

import scala.math._

object IndexedTransformation {

  def fromIndexToCircle(indexVector: IndexVector): Option[Vector] = fromSquareToCircle(indexVector)
  def fromCircleToIndex(circleVector: Vector): IndexVector = fromCircleToSquare(circleVector)

  def centeredNCubeSurface(dimension: Int, radius: Int): Iterable[IndexVector] = {
    centeredNCube(dimension - 1, radius - 1).flatMap(indexVector => {
      (0 until dimension).flatMap(i => {
        Seq(-1.0, +1.0).map(_ * radius).map(u => {
          indexVector.vector.insert(i, u)
        })
      })
    })
  }

  def circle(dimension: Int, radius: Int): Iterable[Vector] = {
    centeredNCubeSurface(dimension, radius).flatMap(fromIndexToCircle)
  }

  def circleWithIndex(dimension: Int, radius: Int): Iterable[(Vector, IndexVector)] = {
    circle(dimension, radius).map(circleVector => (circleVector, fromCircleToIndex(circleVector)))
  }

  /*
  def mainTest(args: Array[String]): Unit = {
    println(circle(8, 4))
  }
  */

  def circleTest(dimension: Int, radius: Int): Unit = {
    val result = circle(dimension, radius)
    println(dimension, radius, result.size)
  }

  def inverseCircleTest(indexVector: IndexVector): Option[Double] = {
    val circleVectorOption = fromIndexToCircle(indexVector)
    circleVectorOption.map(circleVector => {
      val recoveredIndexVector = fromCircleToIndex(circleVector)
      norm(recoveredIndexVector.vector - indexVector)
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

  def neighbourhood(indexVector: IndexVector): Seq[IndexVector] = {
    val vector = indexVector.vector
    val dimension = vector.dimension
    val faceMaxMagnitude = MaxMagnitude(vector)
    val sectorMaxMagnitude = MaxMagnitude(faceMaxMagnitude.fullSpaceRemainder)
    val faceIndex = faceMaxMagnitude.index
    val sectorIndex = sectorMaxMagnitude.index
    (0 until dimension).filterNot(_ == faceIndex).flatMap(i => {
      Seq(-1.0, +1.0).flatMap(u => {
        val neighbour = vector.replace(i, vector(i) + u)
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
    }).map[IndexVector](iv => iv)
  }

}
