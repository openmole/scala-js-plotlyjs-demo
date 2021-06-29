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

  def backToFullSpace(maxMagnitude: MaxMagnitude, newRemainderSpaceRemainder: Vector): Vector = {
    val (left, right) = newRemainderSpaceRemainder.splitAt(maxMagnitude.index)
    left ++ Seq(maxMagnitude.coordinate) ++ right
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

      val radiusOnCellAdjustment = nSphereRadiusRegularization(dimensionNormalization(radiusOnCell)) / radiusOnCell
      val cellCellAdjustment = nSphereRadiusProjection(nSphereRadiusRegularization(radiusOnCell)) / radiusOnCell

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
          val circleVector = backToFullSpace(squareVectorMaxMagnitude, adjustedCircleCellVector).toNorm(nSphereRadius)
          Option(circleVector)
        }
      }
    }
  }

  def mainTest(args: Array[String]): Unit = {
    println(Data.centeredNCube(3, 4, hollow = true).map(RestrictedSpaceTransformation.fromSquareToCircle).filter(_.nonEmpty).map(_.get).filter(_.head >= 0))
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

  def fromCircleToSquare() = {

  }

}
