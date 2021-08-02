package plotlyjs.demo.directions.restrictedspacetransformation

import plotlyjs.demo.utils.Data
import plotlyjs.demo.utils.Vectors._

import scala.math._

object RST1 {

  case class MaxMagnitude(vector: Vector) {
    lazy val index: Int = vector.map(math.abs).zipWithIndex.maxBy(_._1)._2
    lazy val coordinate: Double = vector(index)
    //lazy val signum: Double = scala.math.signum(coordinate)
    lazy val value: Double = abs(coordinate)
    //lazy val fullSpaceComponent: Vector = vector.zipWithIndex.map { case (c, i) => if(i == index) c else 0 }
    //lazy val fullSpaceRemainder: Vector = vector - fullSpaceComponent
    lazy val remainderSpaceRemainder: Vector = vector.zipWithIndex.filterNot(_._2 == index).map(_._1)
  }

  def normalization(dimension: Int)(radiusOnCell: Double): Double = {
    radiusOnCell * sqrt(dimension - 1)
  }

  def regularization(nSphereRadius: Double)(radiusOnCell: Double): Double = {
    nSphereRadius * tan(radiusOnCell / nSphereRadius * Pi / 4)
  }

  def projection(nSphereRadius: Double)(radiusOnCell: Double): Double = {
    nSphereRadius * sin(atan(radiusOnCell / nSphereRadius))
  }

  //include in MaxMagnitude ?
  def backToFullSpace(maxMagnitude: MaxMagnitude, newRemainderSpaceRemainder: Vector): Vector = {
    val (left, right) = newRemainderSpaceRemainder.splitAt(maxMagnitude.index)
    left ++ Seq(maxMagnitude.coordinate) ++ right
  }

  def project(vector: Vector): Vector = {
    val maxMagnitude = MaxMagnitude(vector)
    val radius = maxMagnitude.value
    vector.toNorm(radius)
  }

  def fromSquareToCircle(squareVector: Vector): Option[Vector] = {
    val dimension = squareVector.dimension
    if (dimension == 1) {
      Option(squareVector)
    } else {
      val squareVectorMaxMagnitude = MaxMagnitude(squareVector)
      val nSphereRadius = squareVectorMaxMagnitude.value
      val cellVector = squareVectorMaxMagnitude.remainderSpaceRemainder
      val cellVectorMaxMagnitude = MaxMagnitude(cellVector)
      val radiusOnCell = cellVectorMaxMagnitude.value

      val dimensionNormalization = normalization(dimension)(_)
      val nSphereRadiusRegularization = regularization(nSphereRadius)(_)
      val nSphereRadiusProjection = projection(nSphereRadius)(_)

      val cellCellAdjustment = nSphereRadiusProjection(nSphereRadiusRegularization(radiusOnCell)) / radiusOnCell
      val adjustedRadiusOnCell = nSphereRadiusRegularization(dimensionNormalization(radiusOnCell))

      val recursionInput = backToFullSpace(cellVectorMaxMagnitude, cellVectorMaxMagnitude.remainderSpaceRemainder.scale(1 / cellCellAdjustment))
      val circleCellVectorOption = fromSquareToCircle(recursionInput)
      if (circleCellVectorOption.isEmpty) {
        None
      } else {
        val adjustedCircleCellVector = circleCellVectorOption.get.toNorm(adjustedRadiusOnCell)
        val adjustedCircleCellVectorMaxMagnitude = MaxMagnitude(adjustedCircleCellVector)
        if (adjustedCircleCellVectorMaxMagnitude.value > nSphereRadius || adjustedCircleCellVectorMaxMagnitude.index != cellVectorMaxMagnitude.index) {
          None
        } else {
          val circleVector = project(backToFullSpace(squareVectorMaxMagnitude, adjustedCircleCellVector))
          Option(circleVector)
        }
      }
    }
  }

  def inverseProject(vector: Vector): Vector = {
    val radius = vector.norm
    val maxMagnitude = MaxMagnitude(vector)
    (radius / maxMagnitude.value) * vector
  }

  def inverseProjection(nSphereRadius: Double)(radiusOnCell: Double): Double = {
    tan(asin(radiusOnCell / nSphereRadius)) * nSphereRadius
  }

  def inverseRegularization(nSphereRadius: Double)(radiusOnCell: Double): Double = {
    atan(radiusOnCell / nSphereRadius) * nSphereRadius / (Pi / 4)
  }

  def inverseNormalization(dimension: Int)(radiusOnCell: Double): Double = {
    radiusOnCell / sqrt(dimension - 1)
  }

  def fromCircleToSquare(circleVector: Vector): Vector = {
    val dimension = circleVector.dimension
    if (dimension == 1) {
      circleVector
    } else {
      val nSphereRadius = circleVector.norm

      val inverseProjectCircleVectorMaxMagnitude = MaxMagnitude(inverseProject(circleVector))
      val adjustedCircleCellVector = inverseProjectCircleVectorMaxMagnitude.remainderSpaceRemainder
      val adjustedRadiusOnCell = adjustedCircleCellVector.norm

      val nSphereRadiusInverseProjection = inverseProjection(nSphereRadius)(_)
      val nSphereRadiusInverseRegularization = inverseRegularization(nSphereRadius)(_)
      val dimensionInverseNormalization = inverseNormalization(dimension)(_)

      val radiusOnCell = dimensionInverseNormalization(nSphereRadiusInverseRegularization(adjustedRadiusOnCell))
      val projectedRadius = MaxMagnitude(circleVector).remainderSpaceRemainder.norm
      val inverseCellCellAdjustment = nSphereRadiusInverseRegularization(nSphereRadiusInverseProjection(projectedRadius)) / projectedRadius

      val circleCellVector = adjustedCircleCellVector.toNorm(radiusOnCell)
      val recursionOutput = fromCircleToSquare(circleCellVector)
      val recursionOutputMaxMagnitude = MaxMagnitude(recursionOutput)
      val cellVector = backToFullSpace(recursionOutputMaxMagnitude, recursionOutputMaxMagnitude.remainderSpaceRemainder.scale(1 / inverseCellCellAdjustment))

      val squareVector = backToFullSpace(inverseProjectCircleVectorMaxMagnitude, cellVector)

      squareVector
    }
  }

  def fromSquareToCircle(squareVectors: Seq[Vector]): Seq[Vector] = {
    squareVectors.map(fromSquareToCircle).filter(_.nonEmpty).map(_.get)
  }

  def mainTest(args: Array[String]): Unit = {
    //fromSquareToCircle(Data.centeredNCube(3, 32, hollow = true)).map(fromCircleToSquare)

    val p = 4
    for (dimension <- 1 to 8) {
      val result = RST1.fromSquareToCircle(Data.centeredNCube(dimension, p, hollow = true))
      println(dimension, result.size)
    }
    //TODO Fix that with a space adjustment ? From cube to sphere with a constant area ?

  }

}
