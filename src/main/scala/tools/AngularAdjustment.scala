package tools

import Vectors._
import tools.AngularAdjustment.SpaceSegmentation.SpaceSegmentation

object AngularAdjustment {

  object SpaceSegmentation {

    trait SpaceSegmentation {
      def radialSplit(vector: Seq[Double]): (Seq[Double], Seq[Double])
      def borderNormalSplit(vector: Seq[Double]): (Seq[Double], Seq[Double])
    }

    val cubic: SpaceSegmentation = new SpaceSegmentation {
      def maxMagnitudeDecomposition(v: Seq[Double]): (Seq[Double], Seq[Double]) = {
        val maxMagnitudeIndex = v.map(math.abs).zipWithIndex.maxBy(_._1)._2
        val component = v.zipWithIndex.map { case (c, i) => if(i == maxMagnitudeIndex) c else 0 }
        val remainder = v.zipWithIndex.map { case (c, i) => if(i != maxMagnitudeIndex) c else 0 }
        (component, remainder)
      }
      override def radialSplit(vector: Seq[Double]): (Seq[Double], Seq[Double]) = maxMagnitudeDecomposition(vector)
      override def borderNormalSplit(vector: Seq[Double]): (Seq[Double], Seq[Double]) = maxMagnitudeDecomposition(vector)
    }

    //TODO simplex ?
  }

  def angularAdjustment(spaceSegmentation: SpaceSegmentation, vector: Seq[Double]): Seq[Double] = {
    val (componentToKeep, remainderToAdjust) = spaceSegmentation.radialSplit(vector)
    val orthogonalRadius = length(componentToKeep)
    val radialDirection = normalize(componentToKeep)

    val (borderNormalComponent, _) = spaceSegmentation.borderNormalSplit(remainderToAdjust)
    val centerToBorderProportion = length(borderNormalComponent) / orthogonalRadius

    val touchingBorderRemainder = scale(remainderToAdjust, 1/centerToBorderProportion)
    val touchingBorder = add(componentToKeep, touchingBorderRemainder)
    val maxAngle = angle(touchingBorder, radialDirection)

    val newVectorAngle = centerToBorderProportion * maxAngle

    val newRemainderLength = orthogonalRadius * math.tan(newVectorAngle)
    val adjustedRemainder = toLength(remainderToAdjust, newRemainderLength)
    val adjustedVector = add(componentToKeep, adjustedRemainder)

    if(adjustedVector.count(_.isNaN) == 0) adjustedVector else vector
  }

}
