package plotlyjs.demo.directions

import plotlyjs.demo.utils.Data
import plotlyjs.demo.utils.Vectors._

import scala.math._

object RestrictedSpaceTransformation {

  case class MaxMagnitude(vector: Vector) {
    lazy val index: Int = vector.map(math.abs).zipWithIndex.maxBy(_._1)._2
    lazy val coordinate: Double = vector(index)
    lazy val signum: Double = scala.math.signum(coordinate)
    lazy val value: Double = abs(coordinate)
    lazy val fullSpaceComponent: Vector = vector.zipWithIndex.map { case (c, i) => if(i == index) c else 0 }
    lazy val fullSpaceRemainder: Vector = vector - fullSpaceComponent
    lazy val remainderSpaceRemainder: Vector = vector.zipWithIndex.filterNot(_._2 == index).map(_._1)
  }

  def normalization(dimension: Int)(radiusOnCell: Double): Double = {
    radiusOnCell * sqrt(dimension - 1)
  }

  def regularization(nSphereRadius: Double)(radiusOnCell: Double): Double = {
    nSphereRadius * tan(radiusOnCell/nSphereRadius * Pi/4)
  }

  def projection(nSphereRadius: Double)(radiusOnCell: Double): Double = {
    nSphereRadius * sin(atan(radiusOnCell/nSphereRadius))
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
    if(dimension == 1) {
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
      val radiusOnCellAdjustment = nSphereRadiusRegularization(dimensionNormalization(radiusOnCell)) / radiusOnCell

      val recursionInput = backToFullSpace(cellVectorMaxMagnitude, cellVectorMaxMagnitude.remainderSpaceRemainder.scale(1/cellCellAdjustment))
      val circleCellVectorOption = fromSquareToCircle(recursionInput)
      if(circleCellVectorOption.isEmpty) {
        None
      } else {
        val adjustedCircleCellVector = circleCellVectorOption.get.scale(radiusOnCellAdjustment)
        val adjustedCircleCellVectorMaxMagnitude = MaxMagnitude(adjustedCircleCellVector)
        if(adjustedCircleCellVectorMaxMagnitude.value > nSphereRadius || adjustedCircleCellVectorMaxMagnitude.index != cellVectorMaxMagnitude.index) {
          None
        } else {
          val circleVector = project(backToFullSpace(squareVectorMaxMagnitude, adjustedCircleCellVector))
          Option(circleVector)
        }
      }
    }
  }

  def inverseProject(vector: Vector): Vector = {
    val radius = norm(vector)
    val maxMagnitude = MaxMagnitude(vector)
    (radius/maxMagnitude.value) *: vector
  }

  def inverseProjection(nSphereRadius: Double)(radiusOnCell: Double): Double = {
    tan(asin(radiusOnCell/nSphereRadius)) * nSphereRadius
  }

  def inverseRegularization(nSphereRadius: Double)(radiusOnCell: Double): Double = {
    atan(radiusOnCell/nSphereRadius) * nSphereRadius / (Pi/4)
  }

  def inverseNormalization(dimension: Int)(radiusOnCell: Double): Double = {
    radiusOnCell / sqrt(dimension - 1)
  }

  def fromCircleToSquare(circleVector: Vector): Vector = {
    val dimension = circleVector.dimension
    if(dimension == 1) {
      circleVector
    } else {
      val nSphereRadius = circleVector.norm

      val inverseProjectCircleVectorMaxMagnitude = MaxMagnitude(inverseProject(circleVector))
      val adjustedCircleCellVector = inverseProjectCircleVectorMaxMagnitude.remainderSpaceRemainder
      val adjustedRadiusOnCell = adjustedCircleCellVector.norm

      val nSphereRadiusInverseProjection = inverseProjection(nSphereRadius)(_)
      val nSphereRadiusInverseRegularization = inverseRegularization(nSphereRadius)(_)
      val dimensionInverseNormalization = inverseNormalization(dimension)(_)

      val inverseRadiusOnCellAdjustment = dimensionInverseNormalization(nSphereRadiusInverseRegularization(adjustedRadiusOnCell)) / adjustedRadiusOnCell
      val inverseCellCellAdjustment = nSphereRadiusInverseRegularization(nSphereRadiusInverseProjection(adjustedRadiusOnCell)) / adjustedRadiusOnCell

      val circleCellVector = adjustedCircleCellVector.scale(inverseRadiusOnCellAdjustment)
      val recursionOutput = fromCircleToSquare(circleCellVector)
      val recursionOutputMaxMagnitude = MaxMagnitude(recursionOutput)
      val cellVector = backToFullSpace(recursionOutputMaxMagnitude, recursionOutputMaxMagnitude.remainderSpaceRemainder.scale(1 / inverseCellCellAdjustment))

      val squareVector = backToFullSpace(inverseProjectCircleVectorMaxMagnitude, cellVector)

      squareVector
    }
  }

  def mainTest(args: Array[String]): Unit = {
    val dimension = 3
    val p = 32
    val result = Data
      .centeredNCube(dimension, p, hollow = true)
      .map(RestrictedSpaceTransformation.fromSquareToCircle)
      .filter(_.nonEmpty)
      .map(_.get)
      .map(RestrictedSpaceTransformation.fromCircleToSquare)
      .filter(_.head >= 0)
    println(result)
  }

  /*
  def fromSquareToCircle(squareVector: Vector): Vector = {
    val dimension = squareVector.dimension

    val maxMagnitude = MaxMagnitude(squareVector)
    val squareRemainderInsideCell = maxMagnitude.remainderSpaceRemainder

    val circleRemainderInsideCell = fromSquareToCircle(squareRemainderInsideCell)
    //check for rejection
    val (left, right) = circleRemainderInsideCell.splitAt(maxMagnitude.index)
    val circleRemainderVector = left ++ Seq(maxMagnitude.coordinate) ++ right
    
    val maxAngle = acos(1/(maxMagnitude.value * sqrt(dimension + 1)))
    val angle = maxMagnitude.value * maxAngle
    val radius = maxMagnitude.value * tan(angle)
    circleRemainderVector.toNorm(radius)
  }
  */

  /*
  def fromSquareToCircle(squareVector: Vector): Unit = {
    val dimension = squareVector.dimension

    val maxMagnitude = MaxMagnitude(squareVector)
    val squareRemainderInCell = maxMagnitude.remainderSpaceRemainder
    val cellRadialProportion = MaxMagnitude(squareRemainderInCell).value / maxMagnitude.value
    val maxAngle = acos(1/sqrt(dimension))
    val angle = cellRadialProportion * maxAngle
    val cellRadius = maxMagnitude.value * tan(angle)
    val circleRemainderOnCell = fromSquareToCircle(squareRemainderInCell).scale()


    //normalize
  }
  */

}
