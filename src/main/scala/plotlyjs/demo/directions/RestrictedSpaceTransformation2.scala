package plotlyjs.demo.directions

import plotlyjs.demo.utils.Data
import plotlyjs.demo.utils.Vectors._

import scala.math._

object RestrictedSpaceTransformation2 {

  def regularization(maxAngle: Double)(radiusProportion: Double): Double = {
    tan(radiusProportion * maxAngle)
  }

  def inverseRegularization(maxAngle: Double)(regularizedRadiusProportion: Double): Double = {
    atan(regularizedRadiusProportion) / maxAngle
  }

  def projection(relativeRadius: Double): Double = {
    sin(atan(relativeRadius))
  }

  def inverseProjection(projectedRelativeRadius: Double): Double = {
    tan(asin(projectedRelativeRadius))
  }

  def absoluteFunction(relativeFunction: Double => Double, absoluteReference: Double)(absoluteInput: Double): Double = {
    absoluteReference * relativeFunction(absoluteInput / absoluteReference)
  } // inverse(absoluteFunction(relativeFunction)) = absoluteFunction(inverse(relativeFunction))

  def radiusRegularization(maxAngle: Double, maxRadius: Double)(radius: Double): Double = {
    val absoluteRegularization = absoluteFunction(regularization(maxAngle), maxRadius)(_)
    absoluteRegularization(radius)
  }

  //TODO
  def inverseRadiusRegularization = ???

  def recursionRegularizationFactor(maxAngle: Double, maxRadius: Double, referenceRadius: Double)(radius: Double): Double = {
    val _radiusRegularization = radiusRegularization(maxAngle, maxRadius)(_)
    val absoluteProjection = absoluteFunction(projection, referenceRadius)(_)
    absoluteProjection(_radiusRegularization(radius)) / radius / maxAngle
  }

  //TODO
  def inverseRecursionRegularizationFactor = ???

  /*
  recursionRegularizationFactor(maxAngle, maxRadius, referenceRadius)(radius)
  = absoluteProjection(_radiusRegularization(radius)) / radius / maxAngle
  = absoluteProjection(radiusRegularization(maxAngle, maxRadius)(radius)) / radius / maxAngle
  = absoluteFunction(projection, referenceRadius)(absoluteFunction(regularization(maxAngle), maxRadius)(radius)) / radius / maxAngle
  = referenceRadius * projection(maxRadius * regularization(maxAngle)(radius / maxRadius) / referenceRadius) / radius / maxAngle
  = referenceRadius * sin(atan(maxRadius * tan(radius / maxRadius * maxAngle) / referenceRadius)) / radius / maxAngle
  radius -> 0 => (tan -> sin, atan -> asin)
  -> referenceRadius * sin(asin(maxRadius * sin(radius / maxRadius * maxAngle) / referenceRadius)) / radius / maxAngle
  -> = referenceRadius * maxRadius * sin(radius / maxRadius * maxAngle) / referenceRadius / radius / maxAngle
  -> = maxRadius * sin(radius / maxRadius * maxAngle) / radius / maxAngle
  (x -> 0 => sin(x) -> x), (radius -> 0 => radius / maxRadius * maxAngle -> 0)
  -> maxRadius * radius / maxRadius * maxAngle / radius / maxAngle
  -> = 1
  recursionRegularizationFactor(math.Pi/4, 8, 6)(0.0000001) = 1.0000000000000002
  */

}
