package plotlyjs.demo.directions.restrictedspacetransformation.v4

import plotlyjs.demo.directions.restrictedspacetransformation.v4.IndexVectors._
import plotlyjs.demo.directions.restrictedspacetransformation.v4.Transformation._
import plotlyjs.demo.utils.Vectors._

import scala.math._

object IndexedTransformation {

  def fromIndexToCircle(indexVector: IndexVector): Option[Vector] = {
    fromSquareToCircle(indexVector)
  }

  def fromCircleToIndex(circleVector: Vector): IndexVector = {
    fromCircleToSquare(circleVector)
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
      Seq(-1, +1).flatMap(u => {
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
    }).map[IndexVector](v => v)
  }

}
