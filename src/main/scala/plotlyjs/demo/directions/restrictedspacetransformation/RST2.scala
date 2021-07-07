package plotlyjs.demo.directions.restrictedspacetransformation

import plotlyjs.demo.utils.Data
import plotlyjs.demo.utils.Vectors._

import scala.math._

object RST2 {

  private def assertProportion(proportion: Double): Unit = assert(0 <= proportion && proportion <= 1)

  def regularization(maxAngle: Double)(radiusOnFaceProportion: Double): Double = {
    assertProportion(radiusOnFaceProportion)
    tan(radiusOnFaceProportion * maxAngle) / tan(maxAngle)
  }

  def inverseRegularization(maxAngle: Double)(regularizedRadiusOnFaceProportion: Double): Double = {
    assertProportion(regularizedRadiusOnFaceProportion)
    atan(regularizedRadiusOnFaceProportion * tan(maxAngle)) / maxAngle
  }

  /*
  def projection()(relativeRegularizedRadiusOnFace: Double): Double = {
    sin(atan(relativeRegularizedRadiusOnFace))
  }

  def inverseProjection(projectedRelativeRegularizedRadius: Double): Double = {
    tan(asin(projectedRelativeRegularizedRadius))
  }
  */

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

  def radiusProjection(nCubeRadius: Double, nSphereRadius: Double)(regularizeRadiusOnFace: Double): Double = {
    //val absoluteProjection = absoluteFunction(projection, nCubeRadius)(_)
    //absoluteProjection(regularizeRadiusOnFace)
    sin(atan(regularizeRadiusOnFace / nCubeRadius)) * nSphereRadius
  }

  def inverseRadiusProjection(nCubeRadius: Double, nSphereRadius: Double)(projectedRegularizedRadius: Double): Double = {
    //val inverseAbsoluteProjection = absoluteFunction(inverseProjection, nSphereRadius)(_)
    //inverseAbsoluteProjection(projectedRegularizedRadius)
    tan(asin(projectedRegularizedRadius / nSphereRadius)) * nCubeRadius
  }

  def spaceRegularization(maxAngle: Double, maxRadiusOnFace: Double, nCubeRadius: Double, nSphereRadius: Double)(radiusOnFace: Double): Double = {
    val radiusRegularization = RST2.radiusRegularization(maxAngle, maxRadiusOnFace)(_)
    val radiusProjection = RST2.radiusProjection(nCubeRadius, nSphereRadius)(_)
    radiusProjection(radiusRegularization(radiusOnFace))
  }

  def inverseSpaceRegularization(maxAngle: Double, maxRadiusOnFace: Double, nCubeRadius: Double, nSphereRadius: Double)(projectedRegularizedRadius: Double): Double = {
    val inverseRadiusRegularization = RST2.inverseRadiusRegularization(maxAngle, maxRadiusOnFace)(_)
    val inverseRadiusProjection = RST2.inverseRadiusProjection(nCubeRadius, nSphereRadius)(_)
    inverseRadiusRegularization(inverseRadiusProjection(projectedRegularizedRadius))
  }

  def spaceRegularizationNoLoss(maxAngle: Double, maxRadiusOnFace: Double, nCubeRadius: Double, nSphereRadius: Double)(radiusOnFace: Double): Double = {
    val spaceRegularization = RST2.spaceRegularization(maxAngle, maxRadiusOnFace, nCubeRadius, nSphereRadius)(_)
    //val radiusProjection = RestrictedSpaceTransformation2.radiusProjection(nCubeRadius, nSphereRadius)(_)
    spaceRegularization(radiusOnFace) / spaceRegularization(maxRadiusOnFace)
  }

  def inverseSpaceRegularizationNoLoss(maxAngle: Double, maxRadiusOnFace: Double, nCubeRadius: Double, nSphereRadius: Double)(noLossProjectedRegularizedRadius: Double): Double = {
    val inverseSpaceRegularization = RST2.inverseSpaceRegularization(maxAngle, maxRadiusOnFace, nCubeRadius, nSphereRadius)(_)
    val spaceRegularization = RST2.spaceRegularization(maxAngle, maxRadiusOnFace, nCubeRadius, nSphereRadius)(_)
    inverseSpaceRegularization(noLossProjectedRegularizedRadius * spaceRegularization(maxRadiusOnFace))
  }

  /*
  @deprecated
  def factorFunction(function: Double => Double)(input: Double): Double = {
    function(input) / input
  }

  @deprecated
  def spaceRegularizationFactor(maxAngle: Double, maxRadiusOnFace: Double, nSphereRadius: Double)(radiusOnFace: Double): Double = {
    factorFunction(spaceRegularization(maxAngle, maxRadiusOnFace, nSphereRadius))(radiusOnFace)
  }

  @deprecated
  def inverseSpaceRegularizationFactor(maxAngle: Double, maxRadiusOnFace: Double, nSphereRadius: Double)(regularizedRadiusOnFace: Double): Double = {
    factorFunction(inverseSpaceRegularization(maxAngle, maxRadiusOnFace, nSphereRadius))(regularizedRadiusOnFace)
  }

  @deprecated
  def spaceRegularizationFactorZeroConsistent(maxAngle: Double, maxRadiusOnFace: Double, nSphereRadius: Double)(radiusOnFace: Double): Double = {
    spaceRegularizationFactor(maxAngle, maxRadiusOnFace, nSphereRadius)(radiusOnFace) / maxAngle
  }

  @deprecated
  def inverseSpaceRegularizationFactorZeroConsistent(maxAngle: Double, maxRadiusOnFace: Double, nSphereRadius: Double)(projectedRadius: Double): Double = {
    inverseSpaceRegularizationFactor(maxAngle, maxRadiusOnFace, nSphereRadius)(projectedRadius) * maxAngle
  }
  */

  def computeMaxAngle(nSphereRadius: Double, maxRadiusOnFace: Double): Double = {
    atan(maxRadiusOnFace / nSphereRadius)
  }

  def trigonometryTest(): Unit = {
    val nCubeRadius = 42
    val nSphereRadius = nCubeRadius
    val maxRadiusOnFace = nSphereRadius
    val maxAngle = computeMaxAngle(nSphereRadius, maxRadiusOnFace)

    println(0.42, " " + inverseRegularization(maxAngle)(regularization(maxAngle)(0.42)))
    println(1.42, " " + inverseRadiusProjection(nCubeRadius, nSphereRadius)(radiusProjection(nCubeRadius, nSphereRadius)(1.42)))
    println(0.42 * maxRadiusOnFace, " " + inverseRadiusRegularization(maxAngle, maxRadiusOnFace)(radiusRegularization(maxAngle, maxRadiusOnFace)(0.42 * maxRadiusOnFace)))
    println(0.42 * maxRadiusOnFace, " " + inverseSpaceRegularization(maxAngle, maxRadiusOnFace, nCubeRadius, nSphereRadius)(spaceRegularization(maxAngle, maxRadiusOnFace, nCubeRadius, nSphereRadius)(0.42 * maxRadiusOnFace)))
    println(0.42 * maxRadiusOnFace, " " + inverseSpaceRegularizationNoLoss(maxAngle, maxRadiusOnFace, nCubeRadius, nSphereRadius)(spaceRegularizationNoLoss(maxAngle, maxRadiusOnFace, nCubeRadius, nSphereRadius)(0.42 * maxRadiusOnFace)))
    val almostZero = 0.0000001
    //println(maxAngle, " " + spaceRegularizationFactor(maxAngle, maxRadiusOnFace, nSphereRadius)(almostZero))
    //println(1/maxAngle, " " + inverseSpaceRegularizationFactor(maxAngle, maxRadiusOnFace, nSphereRadius)(almostZero))
    //println(1, " " + spaceRegularizationFactorZeroConsistent(maxAngle, maxRadiusOnFace, nSphereRadius)(almostZero))
    //println(1, " " + inverseSpaceRegularizationFactorZeroConsistent(maxAngle, maxRadiusOnFace, nSphereRadius)(almostZero))
  }

  case class MaxMagnitude(vector: Vector) {
    lazy val index: Int = vector.map(abs).zipWithIndex.maxBy(_._1)._2
    lazy val coordinate: Double = vector(index)
    lazy val value: Double = abs(coordinate)

    lazy val indices: Seq[Int] = vector.map(abs).zipWithIndex.filter(_._1 == value).map(_._2)

    lazy val remainderSpaceRemainder: Vector = vector.zipWithIndex.filterNot(_._2 == index).map(_._1)

    def reconnect(newRemainderSpaceRemainder: Vector): Seq[Double] = {
      val (left, right) = newRemainderSpaceRemainder.splitAt(index)
      left ++ Seq(coordinate) ++ right
    }
  }

  def radiusFromSquareToCircle(dimension: Int, squareRadiusOnFace: Double): Double = {
    //sqrt(dimension) * squareRadiusOnFace
    squareRadiusOnFace
  }

  def vectorFromSquareToCircle(squareVector: Vector): Vector = {
    val squareVectorMaxMagnitude = MaxMagnitude(squareVector)
    val squareRadius = squareVectorMaxMagnitude.value
    val circleRadius = radiusFromSquareToCircle(dimension(squareVector), squareRadius)
    squareVector.toNorm(circleRadius)
  }

  def fromSquareToCircle(squareVector: Vector, tab: Int = 0): Option[Vector] = {
    def tabPrintln(text: String = ""): Unit = {} //println(" ".repeat(tab) + text)

    tabPrintln(s"squareVector = $squareVector")
    val squareVectorDimension = squareVector.dimension
    if (squareVectorDimension == 1) Some(squareVector) else {
      val squareVectorMaxMagnitude = MaxMagnitude(squareVector)
      val nCubeRadius = squareVectorMaxMagnitude.value
      val nSphereRadius = radiusFromSquareToCircle(squareVectorDimension, nCubeRadius)
      val maxSquareRadiusOnFace = nCubeRadius
      val maxCircleRadiusOnFace = radiusFromSquareToCircle(squareVectorDimension - 1, maxSquareRadiusOnFace)
      val maxAngle = computeMaxAngle(nCubeRadius, maxCircleRadiusOnFace)

      val squareVectorOnFace = squareVectorMaxMagnitude.remainderSpaceRemainder
      val squareVectorOnFaceMaxMagnitude = MaxMagnitude(squareVectorOnFace)
      val squareRadiusOnFace = squareVectorOnFaceMaxMagnitude.value

      if (squareRadiusOnFace == 0) Some(squareVectorOnFace) else {
        val circleRadiusOnFace = radiusFromSquareToCircle(dimension(squareVectorOnFace), squareRadiusOnFace)

        val spaceRegularizationRadius = spaceRegularizationNoLoss(maxAngle, maxCircleRadiusOnFace, nCubeRadius, nSphereRadius)(circleRadiusOnFace)
        val spaceContractionFactor = /*min(*/ spaceRegularizationRadius / circleRadiusOnFace /*, 1)*/
        val regularizedCircleRadiusOnFace = radiusRegularization(maxAngle, maxCircleRadiusOnFace)(circleRadiusOnFace)
        assertProportion(regularizedCircleRadiusOnFace / circleRadiusOnFace)

        val recursionInput = squareVectorOnFaceMaxMagnitude.reconnect(squareVectorOnFaceMaxMagnitude.remainderSpaceRemainder.scale(1 / spaceContractionFactor))
        tabPrintln(s"recursionInput = $recursionInput")
        fromSquareToCircle(recursionInput, tab + 1).map(circleVectorOnFace => {
          tabPrintln(s"circleVectorOnFace = $circleVectorOnFace")
          val regularizedCircleVectorOnFace = circleVectorOnFace.toNorm(regularizedCircleRadiusOnFace)
          tabPrintln(s"regularizedCircleVectorOnFace = $regularizedCircleVectorOnFace")

          //tabPrintln(s"regularizedCircleVectorOnFace : ${regularizedCircleVectorOnFace.norm} <= $maxCircleRadiusOnFace ?")
          //assert(circleVectorOnFace.norm <= maxCircleRadiusOnFace)

          regularizedCircleVectorOnFace
        })
      }.flatMap(regularizedCircleVectorOnFace => {
        val regularizedCircleVectorOnFaceMaxMagnitude = MaxMagnitude(regularizedCircleVectorOnFace)

        //tabPrintln(regularizedCircleVectorOnFaceMaxMagnitude.value)

        val cut = true
        if (cut && (regularizedCircleVectorOnFaceMaxMagnitude.value > nCubeRadius || !regularizedCircleVectorOnFaceMaxMagnitude.indices.contains(squareVectorOnFaceMaxMagnitude.index))) {
          tabPrintln("cut")
          println("cut")
          None
        } else {
          val onFaceRegularizedSquareVector = squareVectorMaxMagnitude.reconnect(regularizedCircleVectorOnFace)
          tabPrintln(s"onFaceRegularizedSquareVector = $onFaceRegularizedSquareVector")

          //tabPrintln(nCubeRadius, MaxMagnitude(onFaceRegularizedSquareVector).value)

          val circleVector = vectorFromSquareToCircle(onFaceRegularizedSquareVector)
          tabPrintln(s"circleVector = $circleVector")

          //tabPrintln("circleVector : " + circleVector.norm + " <= " + nSphereRadius + " ?")
          //assert(circleVector.norm <= nSphereRadius)

          Some(circleVector)
        }
      })
    }
  }

  def fromSquareToCircle(squareVectors: Seq[Vector]): Seq[Vector] = {
    squareVectors.map(fromSquareToCircle(_)).filter(_.nonEmpty).map(_.get)
  }

  def fromSquareToCircleTest(): Unit = {
    val p = 4
    for (dimension <- 1 to 5) {
      println(s"dimension = $dimension")

      val result = fromSquareToCircle(Data.centeredNCube(dimension, p, hollow = true))
      println(dimension, result.size)
    }

  }

}
