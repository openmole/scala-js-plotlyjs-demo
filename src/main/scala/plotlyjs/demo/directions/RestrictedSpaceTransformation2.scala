package plotlyjs.demo.directions

import plotlyjs.demo.utils.Data
import plotlyjs.demo.utils.Vectors._

import scala.math._

object RestrictedSpaceTransformation2 {

  def regularization(maxAngle: Double)(radiusOnFaceProportion: Double): Double = {
    tan(radiusOnFaceProportion * maxAngle)
  }

  def inverseRegularization(maxAngle: Double)(regularizedRadiusOnFaceProportion: Double): Double = {
    atan(regularizedRadiusOnFaceProportion) / maxAngle
  }

  def projection(relativeRadiusOnFace: Double): Double = {
    sin(atan(relativeRadiusOnFace))
  }

  def inverseProjection(projectedRelativeRadius: Double): Double = {
    tan(asin(projectedRelativeRadius))
  }

  def absoluteFunction(relativeFunction: Double => Double, absoluteReference: Double)(absoluteInput: Double): Double = {
    absoluteReference * relativeFunction(absoluteInput / absoluteReference)
  } // inverse(absoluteFunction(relativeFunction)) = absoluteFunction(inverse(relativeFunction))

  def radiusRegularization(maxAngle: Double, maxRadiusOnFace: Double)(radiusOnFace: Double): Double = {
    val absoluteRegularization = absoluteFunction(regularization(maxAngle), maxRadiusOnFace)(_)
    absoluteRegularization(radiusOnFace)
  }

  def inverseRadiusRegularization(maxAngle: Double, maxRadiusOnFace: Double)(regularizedRadiusOnFace: Double): Double = {
    val inverseAbsoluteRegularization = absoluteFunction(inverseRegularization(maxAngle), maxRadiusOnFace)(_)
    inverseAbsoluteRegularization(regularizedRadiusOnFace)
  }

  def spaceRegularization(maxAngle: Double, maxRadiusOnFace: Double, nSphereRadius: Double)(radiusOnFace: Double): Double = {
    val radiusRegularization = RestrictedSpaceTransformation2.radiusRegularization(maxAngle, maxRadiusOnFace)(_)
    val absoluteProjection = absoluteFunction(projection, nSphereRadius)(_)
    absoluteProjection(radiusRegularization(radiusOnFace))
  }

  def inverseSpaceRegularization(maxAngle: Double, maxRadiusOnFace: Double, nSphereRadius: Double)(projectedRadius: Double): Double = {
    val inverseRadiusRegularization = RestrictedSpaceTransformation2.inverseRadiusRegularization(maxAngle, maxRadiusOnFace)(_)
    val inverseAbsoluteProjection = absoluteFunction(inverseProjection, nSphereRadius)(_)
    inverseRadiusRegularization(inverseAbsoluteProjection(projectedRadius))
  }

  def factorFunction(function: Double => Double)(input: Double): Double = {
    function(input) / input
  }

  def spaceRegularizationFactor(maxAngle: Double, maxRadiusOnFace: Double, nSphereRadius: Double)(radiusOnFace: Double): Double = {
    factorFunction(spaceRegularization(maxAngle, maxRadiusOnFace, nSphereRadius))(radiusOnFace)
  }

  def inverseSpaceRegularizationFactor(maxAngle: Double, maxRadiusOnFace: Double, nSphereRadius: Double)(regularizedRadiusOnFace: Double): Double = {
    factorFunction(inverseSpaceRegularization(maxAngle, maxRadiusOnFace, nSphereRadius))(regularizedRadiusOnFace)
  }

  def spaceRegularizationFactorZeroConsistent(maxAngle: Double, maxRadiusOnFace: Double, nSphereRadius: Double)(radiusOnFace: Double): Double = {
    spaceRegularizationFactor(maxAngle, maxRadiusOnFace, nSphereRadius)(radiusOnFace) / maxAngle
  }

  def inverseSpaceRegularizationFactorZeroConsistent(maxAngle: Double, maxRadiusOnFace: Double, nSphereRadius: Double)(projectedRadius: Double): Double = {
    inverseSpaceRegularizationFactor(maxAngle, maxRadiusOnFace, nSphereRadius)(projectedRadius) * maxAngle
  }

  def computeMaxAngle(nSphereRadius: Double, maxRadiusOnFace: Double): Double = {
    atan(maxRadiusOnFace / nSphereRadius)
  }

  def trigonometryTest(): Unit = {
    val nSphereRadius = 42
    val maxRadiusOnFace = nSphereRadius
    val maxAngle = computeMaxAngle(nSphereRadius, maxRadiusOnFace)

    println(0.42, " " + inverseRegularization(maxAngle)(regularization(maxAngle)(0.42)))
    println(1.42, " " + inverseProjection(projection(1.42)))
    println(0.42 * maxRadiusOnFace, " " + inverseRadiusRegularization(maxAngle, maxRadiusOnFace)(radiusRegularization(maxAngle, maxRadiusOnFace)(0.42 * maxRadiusOnFace)))
    println(0.42 * maxRadiusOnFace, " " + inverseSpaceRegularization(maxAngle, maxRadiusOnFace, nSphereRadius)(spaceRegularization(maxAngle, maxRadiusOnFace, nSphereRadius)(0.42 * maxRadiusOnFace)))
    val almostZero = 0.0000001
    println(maxAngle, " " + spaceRegularizationFactor(maxAngle, maxRadiusOnFace, nSphereRadius)(almostZero))
    println(1/maxAngle, " " + inverseSpaceRegularizationFactor(maxAngle, maxRadiusOnFace, nSphereRadius)(almostZero))
    println(1, " " + spaceRegularizationFactorZeroConsistent(maxAngle, maxRadiusOnFace, nSphereRadius)(almostZero))
    println(1, " " + inverseSpaceRegularizationFactorZeroConsistent(maxAngle, maxRadiusOnFace, nSphereRadius)(almostZero))
  }

  case class MaxMagnitude(vector: Vector) {
    lazy val index: Int = vector.map(math.abs).zipWithIndex.maxBy(_._1)._2
    lazy val coordinate: Double = vector(index)
    lazy val value: Double = abs(coordinate)
    lazy val remainderSpaceRemainder: Vector = vector.zipWithIndex.filterNot(_._2 == index).map(_._1)
  }

}
