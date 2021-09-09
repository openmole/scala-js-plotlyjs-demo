package plotlyjs.demo.labo.directions.restrictedspacetransformation.v4

import plotlyjs.demo.utils.vector.Vectors._

object Transformation {

  def fromSquareToCircle(squareVector: Vector): Option[Vector] = {
    if(squareVector.dimension == 1) Some(squareVector) else {
      val g = Geometry.fromSquareVector(squareVector)

      val squareVectorMaxMagnitude = MaxMagnitude(squareVector)
      val squareVectorOnFace = squareVectorMaxMagnitude.remainderSpaceRemainder

      val squareVectorOnFaceMaxMagnitude = MaxMagnitude(squareVectorOnFace)
      val squareRadiusOnFace = squareVectorOnFaceMaxMagnitude.value
      val radiusComponent = squareVectorOnFaceMaxMagnitude.fullSpaceComponent
      val spaceComponent = squareVectorOnFaceMaxMagnitude.fullSpaceRemainder

      val regularizedRadiusComponent = g.regularization(radiusComponent)
      val adjustedSpaceComponent = g.adjustment(squareRadiusOnFace, spaceComponent)
      val sameSector = Geometry.squareRadius(regularizedRadiusComponent) >= Geometry.squareRadius(adjustedSpaceComponent)

      Option.when(sameSector)({
        val regularizedAndAdjustedSquareVectorOnFace = regularizedRadiusComponent + adjustedSpaceComponent
        fromSquareToCircle(regularizedAndAdjustedSquareVectorOnFace)
      }).flatten.flatMap(circleVectorOnFace => {
        val sameFace = Geometry.squareRadius(circleVectorOnFace) <= g.maxSquareRadius
        Option.when(sameFace)({
          val circleVector = g.projection(circleVectorOnFace)
          circleVector
        })
      })
    }
  }

  def fromCircleToSquare(circleVector: Vector): Vector = {
    if (circleVector.dimension == 1) circleVector else {
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

}
