package plotlyjs.demo.directions.restrictedspacetransformation.v4

import plotlyjs.demo.utils.Data
import plotlyjs.demo.utils.Vectors._

object Transformation {

  def fromSquareToCircle(squareVector: Vector): Option[Vector] = {
    if (dimension(squareVector) == 1) Some(squareVector) else {
      val g = Geometry.fromSquareVector(squareVector)

      val squareVectorMaxMagnitude = MaxMagnitude(squareVector)
      val squareVectorOnFace = squareVectorMaxMagnitude.remainderSpaceRemainder

      val squareVectorOnFaceMaxMagnitude = MaxMagnitude(squareVectorOnFace)
      val squareRadiusOnFace = squareVectorOnFaceMaxMagnitude.value
      val radiusComponent = squareVectorOnFaceMaxMagnitude.fullSpaceComponent
      val spaceComponent = squareVectorOnFaceMaxMagnitude.fullSpaceRemainder

      val regularizedRadiusComponent = g.regularization(radiusComponent)
      val regularizedSquareRadiusOnFace = Geometry.squareRadius(regularizedRadiusComponent)
      val adjustedSpaceComponentOption = g.adjustment(squareRadiusOnFace, spaceComponent, regularizedSquareRadiusOnFace)

      adjustedSpaceComponentOption.flatMap(adjustedSpaceComponent => {
        val regularizedAndAdjustedSquareVectorOnFace = regularizedRadiusComponent + adjustedSpaceComponent
        fromSquareToCircle(regularizedAndAdjustedSquareVectorOnFace)
      }).flatMap(circleVectorOnFace => {
        val sameFace = Geometry.squareRadius(circleVectorOnFace) <= g.maxSquareRadius
        if (sameFace) {
          val circleVector = g.projection(circleVectorOnFace)
          Some(circleVector)
        } else None
      })
    }
  }

  def fromSquareToCircle(squareVectors: Seq[Vector]): Seq[Vector] = {
    squareVectors.map(fromSquareToCircle).filter(_.nonEmpty).map(_.get)
  }

  def fromSquareToCircleTest(dimension: Int, p: Int): Unit = {
    val result = fromSquareToCircle(Data.centeredNCube(dimension, p, hollow = true))
    println(dimension, p, result.size)
  }

  def fromSquareToCircleTest(maxDimension: Int): Unit = {
    for (dimension <- 1 to maxDimension) {
      var p = 1
      var result = Seq[Vector]()
      do {
        p += 1
        result = fromSquareToCircle(Data.centeredNCube(dimension, p, hollow = true))
      } while (result.isEmpty)
      println(dimension, p, result.size)
    }
  }

  def fromCircleToSquare(circleVector: Vector): Vector = {
    if (dimension(circleVector) == 1) circleVector else {
      val g = Geometry.fromCircleVector(circleVector)

      val circleVectorOnFace = g.inverseProjection(circleVector)
      val regularizedAndAdjustedSquareVectorOnFace = fromCircleToSquare(circleVectorOnFace)

      val regularizedAndAdjustedSquareVectorOnFaceMaxMagnitude = MaxMagnitude(regularizedAndAdjustedSquareVectorOnFace)
      val regularizedRadiusComponent = regularizedAndAdjustedSquareVectorOnFaceMaxMagnitude.fullSpaceComponent
      val adjustedSpaceComponent = regularizedAndAdjustedSquareVectorOnFaceMaxMagnitude.fullSpaceRemainder

      val radiusComponent = g.inverseRegularization(regularizedRadiusComponent)
      val squareRadiusOnFace = Geometry.squareRadius(radiusComponent)
      val spaceComponent = g.inverseAdjustment(squareRadiusOnFace, adjustedSpaceComponent)

      val squareVectorOnFace = radiusComponent + spaceComponent
      val squareVector = g.maxMagnitude.reconnect(squareVectorOnFace)

      squareVector
    }
  }

  def fromCircleToSquareTest(squareVector: Vector): Option[Double] = {
    val circleVectorOption = fromSquareToCircle(squareVector)
    circleVectorOption.map(circleVector => {
      val recoveredSquareVector = fromCircleToSquare(circleVector)
      norm(recoveredSquareVector - squareVector)
    })
  }

  def fromCircleToSquareTest(): Unit = {
    val dimension = 3
    val p = 7
    val cubeFaces = Data.centeredNCube(dimension, p, hollow = true)
    cubeFaces.foreach(squareVector => {
      fromCircleToSquareTest(squareVector).foreach(println)
    })
  }

}
