package plotlyjs.demo.directions.restrictedspacetransformation.v4

import plotlyjs.demo.directions.restrictedspacetransformation.v4.Geometry._
import plotlyjs.demo.utils.Data
import plotlyjs.demo.utils.Vectors._

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

  def inverseRegularizationTest(squareVectorOnFace: Vector): Double = {
    norm(inverseRegularization(regularization(squareVectorOnFace)) - squareVectorOnFace)
  }

  def projection(regularizedSquareRadiusOnFace: Double): Double = {
    val radiusFromSquareToCircle = Geometry.radiusFromSquareToCircle(dimension - 1)(_)
    nSphereRadius * sin(atan(
      radiusFromSquareToCircle(regularizedSquareRadiusOnFace) / nCubeRadius
    ))
  }

  def adjustmentFactor(squareRadiusOnFace: Double): Double = {
    projection(regularization(squareRadiusOnFace)) / squareRadiusOnFace
  }

  val adjustmentFactorZeroLimit: Double = {
    // Depends on radiusFromSquareToCircle and radiusFromCircleToSquare.
    sqrt(dimension) * atan(sqrt(dimension - 1))
  }

  def adjustmentProportion(squareRadiusOnFace: Double): Double = {
    adjustmentFactor(squareRadiusOnFace) / adjustmentFactorZeroLimit
  }

  def adjustment(squareRadiusOnFace: Double, spaceComponent: Vector, regularizedSquareRadiusOnFace: Double): Option[Vector] = {
    if (squareRadiusOnFace == 0) Some(spaceComponent) else {
      val spaceFactor = adjustmentProportion(squareRadiusOnFace)
      val adjustedSpaceComponent = (1 / spaceFactor) *: spaceComponent
      if (squareRadius(adjustedSpaceComponent) > regularizedSquareRadiusOnFace) None else {
        Some(adjustedSpaceComponent)
      }
    }
  }

  def inverseAdjustment(squareRadiusOnFace: Double, adjustedSpaceComponent: Vector): Vector = {
    val reverseSpaceFactor = 1 / adjustmentProportion(squareRadiusOnFace)
    val spaceComponent = (1 / reverseSpaceFactor) *: adjustedSpaceComponent
    spaceComponent
  }

  /*
  def inverseAdjustmentTest(regularizedRadiusComponent: Vector): Option[Double] = {
    val adjustedRegularizedSquareVectorOnFaceOption = adjustment(regularizedRadiusComponent)
    adjustedRegularizedSquareVectorOnFaceOption.map(adjustedRegularizedSquareVectorOnFace => {
      norm(inverseAdjustment(adjustedRegularizedSquareVectorOnFace) - regularizedRadiusComponent)
    })
  }
  */

  def projection(circleVectorOnFace: Vector): Vector = {
    val circleOnFaceSquareVector = maxMagnitude.reconnect(circleVectorOnFace)
    val circleVector = toCircleRadius(circleOnFaceSquareVector, nSphereRadius)
    circleVector
  }

  def inverseProjection(circleVector: Vector): Vector = {
    val circleOnFaceSquareVector = toSquareRadius(circleVector, nCubeRadius)
    val circleVectorOnFace = circleOnFaceSquareVector.remove(maxMagnitude.index) //TODO change to remainder ?
    circleVectorOnFace
  }

  def inverseProjectionTest(circleVectorOnFace: Vector): Double = {
    norm(inverseProjection(projection(circleVectorOnFace)) - circleVectorOnFace)
  }

}

object Geometry {

  //Geometry
  def squareRadius(squareVector: Vector): Double = MaxMagnitude(squareVector).value

  def toSquareRadius(vector: Vector, squareRadius: Double): Vector = {
    if (norm(vector) == 0) vector else {
      (squareRadius / Geometry.squareRadius(vector)) *: vector
    }
  }

  def circleRadius(circleVector: Vector): Double = norm(circleVector)

  def toCircleRadius(vector: Vector, circleRadius: Double): Vector = {
    if (norm(vector) == 0) vector else {
      (circleRadius / Geometry.circleRadius(vector)) *: vector
    }
  }

  def radiusFromSquareToCircle(dimension: Int)(squareRadius: Double): Double = {
    squareRadius * sqrt(dimension)
  }

  def radiusFromCircleToSquare(dimension: Int)(circleRadius: Double): Double = {
    circleRadius / sqrt(dimension)
  }

  def vectorFromSquareToCircle(squareVector: Vector): Vector = {
    val squareRadius = Geometry.squareRadius(squareVector)
    val circleRadius = radiusFromSquareToCircle(dimension(squareVector))(squareRadius)
    toCircleRadius(squareVector, circleRadius)
  }

  def vectorFromCircleToSquare(circleVector: Vector): Vector = {
    val circleRadius = Geometry.circleRadius(circleVector)
    val squareRadius = radiusFromCircleToSquare(dimension(circleVector))(circleRadius)
    toSquareRadius(circleVector, squareRadius)
  }
  //

  //Factory
  def fromSquareVector(squareVector: Vector): Geometry = {
    new Geometry(dimension(squareVector), MaxMagnitude(squareVector), squareRadius(squareVector))
  }

  def fromCircleVector(circleVector: Vector): Geometry = {
    val dimension = circleVector.dimension
    val squareVector = vectorFromCircleToSquare(circleVector)
    val squareVectorMaxMagnitude = MaxMagnitude(squareVector)
    new Geometry(dimension, squareVectorMaxMagnitude, squareVectorMaxMagnitude.value)
  }
  //

  //Tests
  def inverseRegularizationTest(): Unit = {
    val dimension = 3
    val p = 8
    val cubeFaces = Data.centeredNCube(dimension, p, hollow = true)
    cubeFaces.foreach(squareVector => {
      val f = fromSquareVector(squareVector)
      val squareVectorOnFace = MaxMagnitude(squareVector).remainderSpaceRemainder
      println(f.inverseRegularizationTest(squareVectorOnFace))
    })
  }

  /*
  def inverseAdjustmentTest(): Unit = {
    val dimension = 3
    val p = 8
    val cubeFaces = Data.centeredNCube(dimension, p, hollow = true)
    cubeFaces.foreach(squareVector => {
      val f = F.fromSquareVector(squareVector)
      val squareVectorOnFace = MaxMagnitude(squareVector).remainderSpaceRemainder
      val regularizedSquareVectorOnFace = f.regularization(squareVectorOnFace)
      f.inverseAdjustmentTest(regularizedSquareVectorOnFace).foreach(println)
    })
  }
  */

  def inverseProjectionTest(): Unit = {
    val dimension = 3
    val p = 8
    val cubeFaces = Data.centeredNCube(dimension, p, hollow = true)
    cubeFaces.foreach(squareVector => {
      val f = fromSquareVector(squareVector)
      val squareVectorOnFace = MaxMagnitude(squareVector).remainderSpaceRemainder
      println(f.inverseProjectionTest(squareVectorOnFace))
    })
  }
  //

}
