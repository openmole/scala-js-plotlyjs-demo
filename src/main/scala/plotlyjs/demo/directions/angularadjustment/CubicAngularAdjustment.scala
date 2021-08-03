package plotlyjs.demo.directions.angularadjustment

import plotlyjs.demo.utils.Vectors._
import plotlyjs.demo.utils.Matrices._

import scala.math._

object CubicAngularAdjustment {

  /*
  def maxMagnitudeDecomposition(vector: Vector): (Vector, Vector) = {
    val maxMagnitudeIndex = vector.map(math.abs).zipWithIndex.maxBy(_._1)._2
    val component = vector.zipWithIndex.map { case (c, i) => if (i == maxMagnitudeIndex) c else 0 }
    val remainder = sub(vector, component)
    (component, remainder)
  }
  */

  def angularAdjustment(vector: Seq[Double]): Seq[Double] = {

    val dimension = vector.dimension

    val maxMagnitudeIndex = vector.map(abs).zipWithIndex.maxBy(_._1)._2
    val radius = vector(maxMagnitudeIndex)

    val spaceIndices = 0 until dimension
    val cellIndices = 0 until dimension - 1

    def cellIndex(spaceIndex: Int) = {
      if (spaceIndex == maxMagnitudeIndex) {
        -1
      } else if (spaceIndex > maxMagnitudeIndex) {
        spaceIndex - 1
      } else {
        spaceIndex
      }
    }

    def spaceIndex(cellIndex: Int) = {
      if (cellIndex >= maxMagnitudeIndex) {
        cellIndex + 1
      } else {
        cellIndex
      }
    }

    val proportions = cellIndices.map(spaceIndex).map(i => abs(vector(i)) / radius)

    val startVectors = cellIndices.map(spaceIndex).map(i => vector.replace(i, 0))
    val stopVectors = cellIndices.map(spaceIndex).map(i => vector.replace(i, signum(_) * radius))
    val totalAngles = cellIndices.map(i => startVectors(i) ^ stopVectors(i))

    val targetedAngles = proportions zip totalAngles map { case (p, tA) => p * tA }

    val computedValues = targetedAngles.map(sin).map(pow(_, 2))
    val invertedCoefficientsMatrix =
      matrix(cellIndices.length, cellIndices.length, { case (i, _) => computedValues(i) / (computedValues.sum - 1) })
        .mapWithIndex { case (i, j, c) => if (i == j) c - 1 else c }
    /*//Test
    val coefficientsMatrix =
      matrix(cellIndices.length, cellIndices.length, { case (i, _) => computedValues(i) })
        .mapWithIndex { case (i, j, c) => if(i == j) c - 1 else c }
    println(coefficientsMatrix * invertedCoefficientsMatrix)
    */
    //
    val constantsVector = cellIndices.map(i => -computedValues(i) * pow(radius, 2))
    val solutions = invertedCoefficientsMatrix mulColumn constantsVector
    val cellAbsoluteCoordinates = solutions.map(sqrt)

    val adjustedVector = spaceIndices.map(i => if (i == maxMagnitudeIndex) vector(i) else signum(vector(i)) * cellAbsoluteCoordinates(cellIndex(i)))

    adjustedVector
  }

}
