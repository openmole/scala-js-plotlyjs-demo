package plotlyjs.demo.directions

import plotlyjs.demo.utils.Data
import plotlyjs.demo.utils.Vectors._

import scala.math._

object RestrictedSpaceTransformation4 {

  case class MaxMagnitude(vector: Vector) {
    lazy val index: Int = vector.map(abs).zipWithIndex.maxBy(_._1)._2
    lazy val coordinate: Double = vector(index)
    lazy val value: Double = abs(coordinate)

    lazy val indices: Seq[Int] = vector.map(abs).zipWithIndex.filter(_._1 == value).map(_._2)

    lazy val remainderSpaceRemainder: Vector = vector.zipWithIndex.filterNot(_._2 == index).map(_._1)
    def reconnect(newRemainderSpaceRemainder: Vector): Vector = {
      newRemainderSpaceRemainder.insert(index, coordinate)
    }
    def applyToRemainder(f: Vector => Vector): Vector = reconnect(f(remainderSpaceRemainder))
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

    def inverseRegularizationTest(squareRadiusOnFace: Double): Double = {
      inverseRegularization(regularization(squareRadiusOnFace)) - squareRadiusOnFace
    }

    def regularization(squareVectorOnFace: Vector): Vector = {
      val squareRadiusOnFace = F.squareRadius(squareVectorOnFace)
      val regularizedSquareRadiusOnFace = regularization(squareRadiusOnFace)
      val regularizedSquareVectorOnFace = F.toSquareRadius(squareVectorOnFace, regularizedSquareRadiusOnFace)
      regularizedSquareVectorOnFace
    }

    def inverseRegularization(regularizedSquareVectorOnFace: Vector): Vector = {
      val regularizedSquareRadiusOnFace = F.squareRadius(regularizedSquareVectorOnFace)
      val squareRadiusOnFace = inverseRegularization(regularizedSquareRadiusOnFace)
      val squareVectorOnFace = F.toSquareRadius(regularizedSquareVectorOnFace, squareRadiusOnFace)
      squareVectorOnFace
    }

    def inverseRegularizationTest(squareVectorOnFace: Vector): Double = {
      norm(inverseRegularization(regularization(squareVectorOnFace))- squareVectorOnFace)
    }

    def projection(regularizedSquareRadiusOnFace: Double): Double = {
      val radiusFromSquareToCircle = F.radiusFromSquareToCircle(dimension - 1)(_)
      nSphereRadius * sin(atan(
        radiusFromSquareToCircle(regularizedSquareRadiusOnFace) / nCubeRadius
      ))
    }

    def projectionFactor(regularizedSquareRadiusOnFace: Double): Double = {
      projection(regularizedSquareRadiusOnFace) / regularizedSquareRadiusOnFace
    }

    val projectionFactorZeroLimit: Double = {
      // Depends on radiusFromSquareToCircle and radiusFromCircleToSquare.
      sqrt(dimension * (dimension - 1))
    }

    def projectionProportion(regularizedSquareRadiusOnFace: Double): Double = {
      projectionFactor(regularizedSquareRadiusOnFace) / projectionFactorZeroLimit
    }

    def adjustment(regularizedSquareVectorOnFace: Vector): Option[Vector] = {
      val regularizedSquareRadiusOnFace = F.squareRadius(regularizedSquareVectorOnFace)
      val spaceFactor = projectionProportion(regularizedSquareRadiusOnFace)
      val regularizedSquareRadiusOnFaceMaxMagnitude = MaxMagnitude(regularizedSquareVectorOnFace)
      val spaceComponent = regularizedSquareRadiusOnFaceMaxMagnitude.remainderSpaceRemainder
      val adjustedSpaceComponent = (1/spaceFactor) *: spaceComponent
      if(F.squareRadius(adjustedSpaceComponent) > regularizedSquareRadiusOnFace) None else {
        val adjustedRegularizedSquareVectorOnFace = regularizedSquareRadiusOnFaceMaxMagnitude.reconnect(adjustedSpaceComponent)
        Some(adjustedRegularizedSquareVectorOnFace)
      }
    }

    def inverseAdjustment(adjustedRegularisedSquareVectorOnFace: Vector): Vector = {
      val regularizedSquareRadiusOnFace = F.squareRadius(adjustedRegularisedSquareVectorOnFace)
      val spaceFactor = 1/projectionProportion(regularizedSquareRadiusOnFace)
      val regularizedSquareVectorOnFace = MaxMagnitude(adjustedRegularisedSquareVectorOnFace).applyToRemainder(scale(1/spaceFactor))
      regularizedSquareVectorOnFace
    }

    def inverseAdjustmentTest(regularizedSquareVectorOnFace: Vector): Option[Double] = {
      val adjustedRegularizedSquareVectorOnFaceOption = adjustment(regularizedSquareVectorOnFace)
      adjustedRegularizedSquareVectorOnFaceOption.map(adjustedRegularizedSquareVectorOnFace => {
        norm(inverseAdjustment(adjustedRegularizedSquareVectorOnFace) - regularizedSquareVectorOnFace)
      })
    }

    def projection(circleVectorOnFace: Vector): Vector = {
      //val circleOnFaceSquareVector = circleVectorOnFace.insert(faceIndex, nCubeRadius)
      val circleOnFaceSquareVector = maxMagnitude.reconnect(circleVectorOnFace)
      val circleVector = F.toCircleRadius(circleOnFaceSquareVector, nSphereRadius)
      circleVector
    }

    def inverseProjection(circleVector: Vector): Vector = {
      val circleOnFaceSquareVector = F.toSquareRadius(circleVector, nCubeRadius)
      val circleVectorOnFace = circleOnFaceSquareVector.remove(maxMagnitude.index)
      circleVectorOnFace
    }

    def inverseProjectionTest(circleVectorOnFace: Vector): Double = {
      norm(inverseProjection(projection(circleVectorOnFace)) - circleVectorOnFace)
    }

  }

  object F {

    def squareRadius(squareVector: Vector): Double = MaxMagnitude(squareVector).value
    def toSquareRadius(vector: Vector, squareRadius: Double): Vector = {
      if(norm(vector) == 0) vector else {
        (squareRadius / F.squareRadius(vector)) *: vector
      }
    }
    def circleRadius(circleVector: Vector): Double = norm(circleVector)
    def toCircleRadius(vector: Vector, circleRadius: Double): Vector = {
      if(norm(vector) == 0) vector else {
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



    def fromSquareVector(squareVector: Vector): F = {
      F(dimension(squareVector), MaxMagnitude(squareVector), squareRadius(squareVector))
    }

    def fromCircleVector(circleVector: Vector): F = {
      val dimension = circleVector.dimension
      val squareVector = vectorFromCircleToSquare(circleVector)
      val squareVectorMaxMagnitude = MaxMagnitude(squareVector)
      F(dimension, squareVectorMaxMagnitude, squareVectorMaxMagnitude.value)
    }

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

  }

  def fromSquareToCircle(squareVector: Vector): Option[Vector] = {
    if(dimension(squareVector) == 1) Some(squareVector) else {
      val f = F.fromSquareVector(squareVector)
      val squareVectorOnFace = MaxMagnitude(squareVector).remainderSpaceRemainder
      val regularizedSquareVectorOnFace = f.regularization(squareVectorOnFace)
      val spaceComponent = MaxMagnitude(squareVectorOnFace).remainderSpaceRemainder
      (if(dimension(spaceComponent) == 0 || norm(spaceComponent) == 0) Some(squareVectorOnFace) else {
        f.adjustment(regularizedSquareVectorOnFace).flatMap(adjustedRegularizedSquareVectorOnFace => {
          fromSquareToCircle(adjustedRegularizedSquareVectorOnFace)
        })
      }).flatMap(circleVectorOnFace => {
        val cut = true
        if(cut && (F.squareRadius(circleVectorOnFace) > f.maxSquareRadius || (MaxMagnitude(circleVectorOnFace).indices intersect MaxMagnitude(squareVectorOnFace).indices).isEmpty)) {
          //tabPrintln(s"cut : ${circleVectorOnFace.vectorToString}")
          None
        } else {
          val circleVector = f.projection(circleVectorOnFace)
          Some(circleVector)
        }
      })
    }
  }

  def fromSquareToCircle(squareVectors: Seq[Vector]): Seq[Vector] = {
    squareVectors.map(fromSquareToCircle).filter(_.nonEmpty).map(_.get)
  }

  def fromSquareToCircleTest(maxDimension: Int): Unit = {
    for(dimension <- 1 to maxDimension) {
      var p = 1
      var result = Seq[Vector]()
      do {
        p += 1
        result = fromSquareToCircle(Data.centeredNCube(dimension, p, hollow = true))
      } while(result.isEmpty)
      println(dimension, p, result.size)
    }
  }

  def fromCircleToSquare(circleVector: Vector): Vector = {
    if(dimension(circleVector) == 1) circleVector else {
      val f = F.fromCircleVector(circleVector)
      //println(circleVector)
      val circleVectorOnFace = f.inverseProjection(circleVector)
      println("inverseProjection : " + norm(f.projection(circleVectorOnFace) - circleVector))
      val adjustedRegularizedSquareVectorOnFace = fromCircleToSquare(circleVectorOnFace)
      val regularizedSquareVectorOnFace: Vector = {
        if(dimension(adjustedRegularizedSquareVectorOnFace) == 1) adjustedRegularizedSquareVectorOnFace else {
          f.inverseAdjustment(adjustedRegularizedSquareVectorOnFace)
        }
      }
      if(dimension(adjustedRegularizedSquareVectorOnFace) > 1) {
        println(adjustedRegularizedSquareVectorOnFace)
        println(regularizedSquareVectorOnFace)
        println("inverseAdjustment : " + norm(f.adjustment(regularizedSquareVectorOnFace).get - adjustedRegularizedSquareVectorOnFace))
      }
      val squareVectorOnFace = f.inverseRegularization(regularizedSquareVectorOnFace)
      println("inverseRegularization : " + norm(f.regularization(squareVectorOnFace) - regularizedSquareVectorOnFace))
      println("reconnect...")
      println(squareVectorOnFace)
      val squareVector = f.maxMagnitude.reconnect(squareVectorOnFace)
      println(squareVector)
      println("...done.")
      squareVector
    }
  }

  def fromCircleToSquareTest(squareVector: Vector): Option[Double] = {
    val circleVectorOption = fromSquareToCircle(squareVector)
    circleVectorOption.map(circleVector => {
      val recoveredSquareVector = fromCircleToSquare(circleVector)
      println(squareVector)
      println(recoveredSquareVector)
      println()
      norm(recoveredSquareVector - squareVector)
    })
  }

  def fromCircleToSquareTest(): Unit = {
    val dimension = 3
    val p = 6
    val cubeFaces = Data.centeredNCube(dimension, p, hollow = true)
    cubeFaces.foreach(squareVector => {
      fromCircleToSquareTest(squareVector).foreach(println)
    })
  }

}
