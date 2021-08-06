package plotlyjs.demo.directions.angularadjustment

import plotlyjs.demo.directions.angularadjustment.AngularAdjustment.Geometry
import plotlyjs.demo.utils.vector.Vectors._

import scala.math._

object DiscAngularSpaceAdjustment /*extends App */ {

  def nSphereSurface(n: Int, r: Double): Double = {
    if (n % 2 == 0) {
      //noinspection ReplaceToWithUntil
      pow(2, n / 2 + 1) * pow(Pi, n / 2) * pow(r, n) / (1 to n - 1 by 2).map(_.toDouble).product
    } else {
      pow(Pi, (n + 1) / 2) * pow(r, n) / (1.0 / 2.0 * (1 to (n - 1) / 2).map(_.toDouble).product)
    }
  }

  def nDimensionalBallVolume(n: Int, r: Double): Double = {
    if (n % 2 == 0) {
      pow(Pi, n / 2) * pow(r, n) / (1 to n / 2).map(_.toDouble).product
    } else {
      pow(2, (n + 1) / 2) * pow(Pi, (n - 1) / 2) * pow(r, n) / (1 to n by 2).map(_.toDouble).product
    }
  }

  def spaceAdjustmentRadiusScaleFactor(dimension: Int) = {
    val n = dimension - 1
    val wholeSpace = nSphereSurface(n, 1)
    val spaceToFill = wholeSpace / (2 * dimension)
    //val availableCubicSpace = math.pow(2, n)
    val availableSphericalSpace = nDimensionalBallVolume(n, 1)
    //println(wholeSpace, spaceToFill, availableCubicSpace, availableSphericalSpace)
    //println(availableSphericalSpace / spaceToFill)
    math.pow(spaceToFill / availableSphericalSpace, 1.0 / dimension)
  }

  def angularAdjustment(vector: Seq[Double]): Seq[Double] = {
    val spaceSegmentation = Geometry.cubic

    val dimension = vector.length

    val (componentToKeep, remainderToAdjust) = spaceSegmentation.radialSplit(vector)
    val radius = componentToKeep.norm
    val radialDirection = componentToKeep.normalize

    val fillingProportion = remainderToAdjust.norm / (radius * spaceAdjustmentRadiusScaleFactor(dimension))
    if (fillingProportion <= 1) {
      val fillingLimit_CenterToBorderProportion = {
        val touchingFillingLimitRemainder = remainderToAdjust.scale( 1 / fillingProportion)
        val (borderNormalComponent, _) = spaceSegmentation.borderNormalSplit(touchingFillingLimitRemainder)
        borderNormalComponent.norm / radius
      }
      val fillingAdjustedRemainder = (1 / fillingLimit_CenterToBorderProportion) * remainderToAdjust

      AngularAdjustment.cellRadialAdjustment(Geometry.cubic, componentToKeep + fillingAdjustedRemainder)
    } else {
      null
    }
  }

  /*
  for(dimension <- 0 to 370) {
    println(s"dimension = $dimension (n = ${dimension - 1})")
    spaceAdjustmentRadiusScaleFactor(dimension)
    println()
  }
  */
}
