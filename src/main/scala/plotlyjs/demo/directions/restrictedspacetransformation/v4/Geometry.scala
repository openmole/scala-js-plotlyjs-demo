package plotlyjs.demo.directions.restrictedspacetransformation.v4

import plotlyjs.demo.directions.restrictedspacetransformation.v4.Geometry._
import plotlyjs.demo.utils.vector.IntVectors
import plotlyjs.demo.utils.vector.IntVectors._
import plotlyjs.demo.utils.vector.Vectors._

import scala.math._

class Geometry(_dimension: Int, _maxMagnitude: MaxMagnitude, _nCubeRadius: Double) {

  val dimension: Int = _dimension
  val maxMagnitude: MaxMagnitude = _maxMagnitude
  val nCubeRadius: Double = _nCubeRadius
  val maxSquareRadius: Double = nCubeRadius
  val nSphereRadius: Double = radiusFromSquareToCircle(dimension)(nCubeRadius)
  val maxCircleRadius: Double = radiusFromSquareToCircle(dimension - 1)(maxSquareRadius)
  val maxAngle: Double = atan(maxCircleRadius / nCubeRadius)

  def regularization(squareRadiusOnFace: Double): Double = {
    val radiusFromSquareToCircle = Geometry.radiusFromSquareToCircle(dimension - 1)(_)
    val radiusFromCircleToSquare = Geometry.radiusFromCircleToSquare(dimension - 1)(_)
    radiusFromCircleToSquare(
      maxCircleRadius * tan(
        maxAngle * radiusFromSquareToCircle(squareRadiusOnFace) / maxCircleRadius
      ) / tan(maxAngle)
    )
  }

  def inverseRegularization(regularizedSquareRadiusOnFace: Double): Double = {
    val radiusFromSquareToCircle = Geometry.radiusFromSquareToCircle(dimension - 1)(_)
    val radiusFromCircleToSquare = Geometry.radiusFromCircleToSquare(dimension - 1)(_)
    radiusFromCircleToSquare(
      maxCircleRadius * atan(
        tan(maxAngle) * radiusFromSquareToCircle(regularizedSquareRadiusOnFace) / maxCircleRadius
      ) / maxAngle
    )
  }

  def inverseRegularizationTest(radiusComponent: Double): Double = {
    inverseRegularization(regularization(radiusComponent)) - radiusComponent
  }

  def regularization(radiusComponent: Vector): Vector = {
    val squareRadiusOnFace = squareRadius(radiusComponent)
    val regularizedSquareRadiusOnFace = regularization(squareRadiusOnFace)
    val regularizedRadiusComponent = toSquareRadius(radiusComponent, regularizedSquareRadiusOnFace)
    regularizedRadiusComponent
  }

  def inverseRegularization(regularizedRadiusComponent: Vector): Vector = {
    val regularizedSquareRadiusOnFace = squareRadius(regularizedRadiusComponent)
    val squareRadiusOnFace = inverseRegularization(regularizedSquareRadiusOnFace)
    val radiusComponent = toSquareRadius(regularizedRadiusComponent, squareRadiusOnFace)
    radiusComponent
  }

  def inverseRegularizationTest(radiusComponent: Vector): Double = {
    (inverseRegularization(regularization(radiusComponent)) - radiusComponent).norm
  }

  def projection(regularizedSquareRadiusOnFace: Double): Double = {
    val radiusFromSquareToCircle = Geometry.radiusFromSquareToCircle(dimension - 1)(_)
    nSphereRadius * sin(atan(
      radiusFromSquareToCircle(regularizedSquareRadiusOnFace) / nCubeRadius
    ))
  }

  val adjustmentFactorZeroLimit: Double = {
    // Depends on radiusFromSquareToCircle and radiusFromCircleToSquare.
    sqrt(dimension) * atan(sqrt(dimension - 1))
  }

  def adjustmentFactor(squareRadiusOnFace: Double): Double = {
    if(squareRadiusOnFace == 0) adjustmentFactorZeroLimit else {
      projection(regularization(squareRadiusOnFace)) / squareRadiusOnFace
    }
  }

  def adjustmentProportion(squareRadiusOnFace: Double): Double = {
    adjustmentFactor(squareRadiusOnFace) / adjustmentFactorZeroLimit
  }

  def adjustment(squareRadiusOnFace: Double, spaceComponent: Vector): Vector = {
    val spaceFactor = adjustmentProportion(squareRadiusOnFace)
    val adjustedSpaceComponent = (1 / spaceFactor) * spaceComponent
    adjustedSpaceComponent
  }

  def inverseAdjustment(squareRadiusOnFace: Double, adjustedSpaceComponent: Vector): Vector = {
    val reverseSpaceFactor = 1 / adjustmentProportion(squareRadiusOnFace)
    val spaceComponent = (1 / reverseSpaceFactor) * adjustedSpaceComponent
    spaceComponent
  }

  def inverseAdjustmentTest(squareRadiusOnFace: Double, spaceComponent: Vector): Double = {
    val adjustedSpaceComponent = adjustment(squareRadiusOnFace, spaceComponent)
    (inverseAdjustment(squareRadiusOnFace, adjustedSpaceComponent) - spaceComponent).norm
  }

  def projection(circleVectorOnFace: Vector): Vector = {
    val circleOnFaceSquareVector = maxMagnitude.reconnect(circleVectorOnFace)
    val circleVector = toCircleRadius(circleOnFaceSquareVector, nSphereRadius)
    circleVector
  }

  def inverseProjection(circleVector: Vector): Vector = {
    val circleOnFaceSquareVector = toSquareRadius(circleVector, nCubeRadius)
    val circleVectorOnFace = circleOnFaceSquareVector.remove(maxMagnitude.index)
    circleVectorOnFace
  }

  def inverseProjectionTest(circleVectorOnFace: Vector): Double = {
    (inverseProjection(projection(circleVectorOnFace)) - circleVectorOnFace).norm
  }

}

object Geometry {

  //Geometry
  def squareRadius(squareVector: Vector): Double = MaxMagnitude(squareVector).value
  def circleRadius(circleVector: Vector): Double = circleVector.norm

  def toSquareRadius(vector: Vector, newRadius: Double): Vector = vector.toNorm(squareRadius, newRadius)
  def toCircleRadius(vector: Vector, newRadius: Double): Vector = vector.toNorm(circleRadius, newRadius)

  def radiusFromSquareToCircle(dimension: Int)(squareRadius: Double): Double = squareRadius * sqrt(dimension)
  def radiusFromCircleToSquare(dimension: Int)(circleRadius: Double): Double = circleRadius / sqrt(dimension)

  def vectorFromSquareToCircle(squareVector: Vector): Vector = {
    val squareRadius = Geometry.squareRadius(squareVector)
    val circleRadius = radiusFromSquareToCircle(squareVector.dimension)(squareRadius)
    toCircleRadius(squareVector, circleRadius)
  }

  def vectorFromCircleToSquare(circleVector: Vector): Vector = {
    val circleRadius = Geometry.circleRadius(circleVector)
    val squareRadius = radiusFromCircleToSquare(circleVector.dimension)(circleRadius)
    toSquareRadius(circleVector, squareRadius)
  }
  //

  //Factory
  def fromSquareVector(squareVector: Vector): Geometry = {
    new Geometry(squareVector.dimension, MaxMagnitude(squareVector), squareRadius(squareVector))
  }

  def fromCircleVector(circleVector: Vector): Geometry = {
    val squareVector = vectorFromCircleToSquare(circleVector)
    fromSquareVector(squareVector)
  }
  //

  //Tests
  def inverseRegularizationTest(): Unit = {
    val dimension = 3
    val radius = 8
    val cubeFaces = IntVectors.centeredNCube(dimension, radius).map(_.vector)
    cubeFaces.foreach(squareVector => {
      val g = fromSquareVector(squareVector)
      val squareVectorOnFace = MaxMagnitude(squareVector).remainderSpaceRemainder
      val radiusComponent = MaxMagnitude(squareVectorOnFace).fullSpaceComponent
      println(g.inverseRegularizationTest(radiusComponent))
    })
  }

  def inverseAdjustmentTest(): Unit = {
    val dimension = 3
    val radius = 8
    val cubeFaces = IntVectors.centeredNCube(dimension, radius).map(_.vector)
    cubeFaces.foreach(squareVector => {
      val g = fromSquareVector(squareVector)
      val squareVectorOnFace = MaxMagnitude(squareVector).remainderSpaceRemainder
      val squareRadiusOnFace = squareRadius(squareVectorOnFace)
      val spaceComponent = MaxMagnitude(squareVectorOnFace).fullSpaceRemainder
      println(g.inverseAdjustmentTest(squareRadiusOnFace, spaceComponent))
    })
  }

  def inverseProjectionTest(): Unit = {
    val dimension = 3
    val radius = 8
    val cubeFaces = IntVectors.centeredNCube(dimension, radius).map(_.vector)
    cubeFaces.foreach(squareVector => {
      val g = fromSquareVector(squareVector)
      val squareVectorOnFace = MaxMagnitude(squareVector).remainderSpaceRemainder
      println(g.inverseProjectionTest(squareVectorOnFace))
    })
  }
  //

}
