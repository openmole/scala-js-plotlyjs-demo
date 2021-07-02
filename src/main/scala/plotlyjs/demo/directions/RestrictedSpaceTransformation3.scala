package plotlyjs.demo.directions

import plotlyjs.demo.utils.Data
import plotlyjs.demo.utils.Vectors._

import scala.math._

object RestrictedSpaceTransformation3 {

  case class MaxMagnitude(vector: Vector) {
    lazy val index: Int = vector.map(abs).zipWithIndex.maxBy(_._1)._2
    lazy val coordinate: Double = vector(index)
    lazy val value: Double = abs(coordinate)

    lazy val indices: Seq[Int] = vector.map(abs).zipWithIndex.filter(_._1 == value).map(_._2)

    lazy val remainderSpaceRemainder: Vector = vector.zipWithIndex.filterNot(_._2 == index).map(_._1)
    def reconnect(newRemainderSpaceRemainder: Vector): Vector = {
      val (left, right) = newRemainderSpaceRemainder.splitAt(index)
      left ++ Seq(coordinate) ++ right
    }
    def applyToRemainder(f: Vector => Vector): Vector = reconnect(f(remainderSpaceRemainder))
  }

  object F {

    def squareRadius(squareVector: Vector): Double = MaxMagnitude(squareVector).value
    def toSquareRadius(squareVector: Vector, squareRadius: Double): Vector = {
      (squareRadius / F.squareRadius(squareVector)) *: squareVector
    }
    def circleRadius(circleVector: Vector): Double = norm(circleVector)
    def toCircleRadius(circleVector: Vector, circleRadius: Double): Vector = {
      (circleRadius / F.circleRadius(circleVector)) *: circleVector
    }

    def radiusFromSquareToCircle(dimension: Int)(squareRadius: Double): Double = squareRadius * sqrt(dimension)
    def radiusFromCircleToSquare(dimension: Int)(circleRadius: Double): Double = circleRadius / sqrt(dimension)

    def vectorFromSquareToCircle(squareVector: Vector): Vector = {
      val squareRadius = F.squareRadius(squareVector)
      val circleRadius = radiusFromSquareToCircle(dimension(squareVector))(squareRadius)
      squareVector.toNorm(circleRadius)
    }

  }

  case class F(_dimension: Int, _nCubeRadius: Double) {

    val dimension: Int = _dimension
    val nCubeRadius: Double = _nCubeRadius
    val maxSquareRadius: Double = nCubeRadius
    val nSphereRadius: Double = F.radiusFromSquareToCircle(dimension)(nCubeRadius)
    val maxCircleRadius: Double = F.radiusFromSquareToCircle(dimension - 1)(maxSquareRadius)
    val maxAngle: Double = atan(maxCircleRadius / nCubeRadius)

    def regularization(squareRadius: Double): Double = {
      val radiusFromSquareToCircle = F.radiusFromSquareToCircle(dimension - 1)(_)
      val radiusFromCircleToSquare = F.radiusFromCircleToSquare(dimension - 1)(_)
      radiusFromCircleToSquare(
        maxCircleRadius * tan(
          maxAngle * radiusFromSquareToCircle(squareRadius) / maxCircleRadius
        ) / tan(maxAngle)
      )
    }

    def projection(squareRadius: Double): Double = {
      val radiusFromSquareToCircle = F.radiusFromSquareToCircle(dimension - 1)(_)
      nSphereRadius * sin(atan(
        radiusFromSquareToCircle(regularization(squareRadius)) / nCubeRadius
      ))
    }

    def projectionFactor(squareRadius: Double): Double = {
      projection(squareRadius) / squareRadius
    }

    val projectionFactorZeroLimit: Double = {
      // Depends on radiusFromSquareToCircle and radiusFromCircleToSquare.
      sqrt(dimension - 1) * sqrt(dimension) * maxAngle / tan(maxAngle)
    }

    def projectionProportion(squareRadius: Double): Double = {
      projectionFactor(squareRadius) / projectionFactorZeroLimit
    }

  }

  def assertProportion(proportion: Double): Unit = {
    val assertion = 0 <= proportion && proportion <= 1
    val epsilon = 0.001
    val looseAssertion = -epsilon + 0 <= proportion && proportion <= 1 + epsilon
    if(!assertion) {
      if(looseAssertion) {
        println(s"assertProportion warning : $proportion")
      } else {
        println(s"assertProportion failed : $proportion")
        assert(false)
      }
    }
  }

  def fromSquareToCircle(squareVector: Vector, tab: Int = 0): Option[Vector] = {
    //TODO squareVector -> cubeVector ?

    def tabPrintln(text: String = ""): Unit = println(" ".repeat(tab) + text)

    val squareVectorDimension = squareVector.dimension
    if(squareVectorDimension == 1) Some(squareVector) else {
      val squareVectorMaxMagnitude = MaxMagnitude(squareVector)
      val nCubeRadius = F.squareRadius(squareVector)
      val f = F(squareVectorDimension, nCubeRadius)

      val squareVectorOnFace = squareVectorMaxMagnitude.remainderSpaceRemainder
      val squareRadiusOnFace = F.squareRadius(squareVectorOnFace)

      if(squareRadiusOnFace == 0) Some(squareVectorOnFace) else {
        val regularizedSquareRadiusOnFace = f.regularization(squareRadiusOnFace)
        //assertProportion(regularizedSquareRadiusOnFace / squareRadiusOnFace)
        val spaceContractionFactor = f.projectionProportion(squareRadiusOnFace)
        //assertProportion(spaceContractionFactor)

        val radiusRegularizedSquareVectorOnFace = F.toSquareRadius(squareVectorOnFace, regularizedSquareRadiusOnFace)
        val radiusAndSpaceRegularizedSquareVectorOnFace = MaxMagnitude(radiusRegularizedSquareVectorOnFace).applyToRemainder(scale(1/spaceContractionFactor))
        fromSquareToCircle(radiusAndSpaceRegularizedSquareVectorOnFace, tab + 1).flatMap(circleVectorOnFace => {
          val cut = true
          //tabPrintln(MaxMagnitude(circleVectorOnFace).indices + " " + MaxMagnitude(squareVectorOnFace).indices)
          if(cut && (F.squareRadius(circleVectorOnFace) > f.maxSquareRadius || (MaxMagnitude(circleVectorOnFace).indices intersect MaxMagnitude(squareVectorOnFace).indices).isEmpty)) {
            //tabPrintln(s"cut : ${circleVectorOnFace.vectorToString}")
            None
          } else {
            val circleOnFaceSquareVector = squareVectorMaxMagnitude.reconnect(circleVectorOnFace)
            val circleVector = F.toCircleRadius(circleOnFaceSquareVector, f.nSphereRadius)
            //tabPrintln(s"circleVector = $circleVector")
            Some(circleVector)
          }
        })
      }
    }
  }

  def fromSquareToCircle(squareVectors: Seq[Vector]): Seq[Vector] = {
    val result = squareVectors.map(fromSquareToCircle(_)).filter(_.nonEmpty).map(_.get)
    println("Number of points : " + result.size)
    result
  }

  def fromSquareToCircleTest(dimension: Int, p: Int): Unit = {
    //val p = 4
    for(_ <- 0 to 0) {
      println(s"dimension = $dimension")
      val result = fromSquareToCircle(Data.centeredNCube(dimension, p, hollow = true))
      println(dimension, result.size)
    }
  }

}
