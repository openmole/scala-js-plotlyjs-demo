package plotlyjs.demo.directions.restrictedspacetransformation

import plotlyjs.demo.utils.Data
import plotlyjs.demo.utils.Vectors._

import scala.math._

object RestrictedSpaceTransformation4 {

  case class MaxMagnitude(vector: Vector) {
    lazy val index: Int = vector.map(abs).zipWithIndex.maxBy(_._1)._2
    lazy val coordinate: Double = vector(index)
    lazy val value: Double = abs(coordinate)
    lazy val fullSpaceComponent: Vector = vector.zipWithIndex.map { case (c, i) => if (i == index) c else 0 }
    lazy val fullSpaceRemainder: Vector = vector - fullSpaceComponent
    lazy val remainderSpaceRemainder: Vector = vector.zipWithIndex.filterNot(_._2 == index).map(_._1)

    def reconnect(newRemainderSpaceRemainder: Vector): Vector = {
      newRemainderSpaceRemainder.insert(index, coordinate)
    }
  }

  case class F(_dimension: Int, _maxMagnitude: MaxMagnitude, _nCubeRadius: Double) {

    val dimension: Int = _dimension
    val maxMagnitude: MaxMagnitude = _maxMagnitude
    val nCubeRadius: Double = _nCubeRadius
    val maxSquareRadius: Double = nCubeRadius
    val nSphereRadius: Double = F.radiusFromSquareToCircle(dimension)(nCubeRadius)
    val maxCircleRadius: Double = F.radiusFromSquareToCircle(dimension - 1)(maxSquareRadius)
    val maxAngle: Double = atan(maxCircleRadius / nCubeRadius)

    def regularization(squareRadiusOnFace: Double): Double = {
      val radiusFromSquareToCircle = F.radiusFromSquareToCircle(dimension - 1)(_)
      val radiusFromCircleToSquare = F.radiusFromCircleToSquare(dimension - 1)(_)
      radiusFromCircleToSquare(
        maxCircleRadius * tan(
          maxAngle * radiusFromSquareToCircle(squareRadiusOnFace) / maxCircleRadius
        ) / tan(maxAngle)
      )
    }

    def inverseRegularization(regularizedSquareRadiusOnFace: Double): Double = {
      val radiusFromSquareToCircle = F.radiusFromSquareToCircle(dimension - 1)(_)
      val radiusFromCircleToSquare = F.radiusFromCircleToSquare(dimension - 1)(_)
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
      val squareRadiusOnFace = F.squareRadius(radiusComponent)
      val regularizedSquareRadiusOnFace = regularization(squareRadiusOnFace)
      val regularizedRadiusComponent = F.toSquareRadius(radiusComponent, regularizedSquareRadiusOnFace)
      regularizedRadiusComponent
    }

    def inverseRegularization(regularizedRadiusComponent: Vector): Vector = {
      val regularizedSquareRadiusOnFace = F.squareRadius(regularizedRadiusComponent)
      val squareRadiusOnFace = inverseRegularization(regularizedSquareRadiusOnFace)
      val radiusComponent = F.toSquareRadius(regularizedRadiusComponent, squareRadiusOnFace)
      radiusComponent
    }

    def inverseRegularizationTest(squareVectorOnFace: Vector): Double = {
      norm(inverseRegularization(regularization(squareVectorOnFace)) - squareVectorOnFace)
    }

    def projection(regularizedSquareRadiusOnFace: Double): Double = {
      val radiusFromSquareToCircle = F.radiusFromSquareToCircle(dimension - 1)(_)
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
        if (F.squareRadius(adjustedSpaceComponent) > regularizedSquareRadiusOnFace) None else {
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
      val circleVector = F.toCircleRadius(circleOnFaceSquareVector, nSphereRadius)
      circleVector
    }

    def inverseProjection(circleVector: Vector): Vector = {
      val circleOnFaceSquareVector = F.toSquareRadius(circleVector, nCubeRadius)
      val circleVectorOnFace = circleOnFaceSquareVector.remove(maxMagnitude.index) //TODO change to remainder ?
      circleVectorOnFace
    }

    def inverseProjectionTest(circleVectorOnFace: Vector): Double = {
      norm(inverseProjection(projection(circleVectorOnFace)) - circleVectorOnFace)
    }

  }

  object F {

    //Geometry
    def squareRadius(squareVector: Vector): Double = MaxMagnitude(squareVector).value

    def toSquareRadius(vector: Vector, squareRadius: Double): Vector = {
      if (norm(vector) == 0) vector else {
        (squareRadius / F.squareRadius(vector)) *: vector
      }
    }

    def circleRadius(circleVector: Vector): Double = norm(circleVector)

    def toCircleRadius(vector: Vector, circleRadius: Double): Vector = {
      if (norm(vector) == 0) vector else {
        (circleRadius / F.circleRadius(vector)) *: vector
      }
    }

    def radiusFromSquareToCircle(dimension: Int)(squareRadius: Double): Double = {
      squareRadius * sqrt(dimension)
    }

    def radiusFromCircleToSquare(dimension: Int)(circleRadius: Double): Double = {
      circleRadius / sqrt(dimension)
    }

    def vectorFromSquareToCircle(squareVector: Vector): Vector = {
      val squareRadius = F.squareRadius(squareVector)
      val circleRadius = radiusFromSquareToCircle(dimension(squareVector))(squareRadius)
      toCircleRadius(squareVector, circleRadius)
    }

    def vectorFromCircleToSquare(circleVector: Vector): Vector = {
      val circleRadius = F.circleRadius(circleVector)
      val squareRadius = radiusFromCircleToSquare(dimension(circleVector))(circleRadius)
      toSquareRadius(circleVector, squareRadius)
    }
    //

    //Factory
    def fromSquareVector(squareVector: Vector): F = {
      F(dimension(squareVector), MaxMagnitude(squareVector), squareRadius(squareVector))
    }

    def fromCircleVector(circleVector: Vector): F = {
      val dimension = circleVector.dimension
      val squareVector = vectorFromCircleToSquare(circleVector)
      val squareVectorMaxMagnitude = MaxMagnitude(squareVector)
      F(dimension, squareVectorMaxMagnitude, squareVectorMaxMagnitude.value)
    }
    //

    //Tests
    def inverseRegularizationTest(): Unit = {
      val dimension = 3
      val p = 8
      val cubeFaces = Data.centeredNCube(dimension, p, hollow = true)
      cubeFaces.foreach(squareVector => {
        val f = F.fromSquareVector(squareVector)
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
        val f = F.fromSquareVector(squareVector)
        val squareVectorOnFace = MaxMagnitude(squareVector).remainderSpaceRemainder
        println(f.inverseProjectionTest(squareVectorOnFace))
      })
    }
    //

  }

  def fromSquareToCircle(squareVector: Vector): Option[Vector] = {
    if (dimension(squareVector) == 1) Some(squareVector) else {
      val f = F.fromSquareVector(squareVector)

      val squareVectorMaxMagnitude = MaxMagnitude(squareVector)
      val squareVectorOnFace = squareVectorMaxMagnitude.remainderSpaceRemainder

      val squareVectorOnFaceMaxMagnitude = MaxMagnitude(squareVectorOnFace)
      val squareRadiusOnFace = squareVectorOnFaceMaxMagnitude.value
      val radiusComponent = squareVectorOnFaceMaxMagnitude.fullSpaceComponent
      val spaceComponent = squareVectorOnFaceMaxMagnitude.fullSpaceRemainder

      val regularizedRadiusComponent = f.regularization(radiusComponent)
      val regularizedSquareRadiusOnFace = F.squareRadius(regularizedRadiusComponent)
      val adjustedSpaceComponentOption = f.adjustment(squareRadiusOnFace, spaceComponent, regularizedSquareRadiusOnFace)

      adjustedSpaceComponentOption.flatMap(adjustedSpaceComponent => {
        val regularizedAndAdjustedSquareVectorOnFace = regularizedRadiusComponent + adjustedSpaceComponent
        fromSquareToCircle(regularizedAndAdjustedSquareVectorOnFace)
      }).flatMap(circleVectorOnFace => {
        val sameFace = F.squareRadius(circleVectorOnFace) <= f.maxSquareRadius
        if (sameFace) {
          val circleVector = f.projection(circleVectorOnFace)
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
      val f = F.fromCircleVector(circleVector)

      val circleVectorOnFace = f.inverseProjection(circleVector)
      val regularizedAndAdjustedSquareVectorOnFace = fromCircleToSquare(circleVectorOnFace)

      val regularizedAndAdjustedSquareVectorOnFaceMaxMagnitude = MaxMagnitude(regularizedAndAdjustedSquareVectorOnFace)
      val regularizedRadiusComponent = regularizedAndAdjustedSquareVectorOnFaceMaxMagnitude.fullSpaceComponent
      val adjustedSpaceComponent = regularizedAndAdjustedSquareVectorOnFaceMaxMagnitude.fullSpaceRemainder

      val radiusComponent = f.inverseRegularization(regularizedRadiusComponent)
      val squareRadiusOnFace = F.squareRadius(radiusComponent)
      val spaceComponent = f.inverseAdjustment(squareRadiusOnFace, adjustedSpaceComponent)

      val squareVectorOnFace = radiusComponent + spaceComponent
      val squareVector = f.maxMagnitude.reconnect(squareVectorOnFace)

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
