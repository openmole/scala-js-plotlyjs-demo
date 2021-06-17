package tools

import Vectors._

object AngularAdjustment {

  def maxMagnitudeDecomposition(v: Seq[Double]): (Seq[Double], Seq[Double]) = {
    val maxMagnitudeIndex = v.map(math.abs).zipWithIndex.maxBy(_._1)._2
    val component = v.zipWithIndex.map { case (c, i) => if(i == maxMagnitudeIndex) c else 0 }
    val remainder = v.zipWithIndex.map { case (c, i) => if(i != maxMagnitudeIndex) c else 0 }
    (component, remainder)
  }

  def angularAdjustment(vector: Seq[Double]): Seq[Double] = {
    val dimension = vector.length

    val (componentToKeep, remainderToAdjust) = maxMagnitudeDecomposition(vector)
    val orthogonalRadius = length(componentToKeep)
    val radialDirection = normalize(componentToKeep)

    val (borderNormalComponent, _) = maxMagnitudeDecomposition(remainderToAdjust)
    val centerToBorderProportion = length(borderNormalComponent) / orthogonalRadius
    //println(s"Is proportion ? ${0 <= centerToBorderProportion && centerToBorderProportion <= 1}")

    val touchingBorderRemainder = scale(remainderToAdjust, 1/centerToBorderProportion)
    //println(s"Expected length : $orthogonalRadius, actual length : ${length(maxMagnitudeDecomposition(touchingBorderRemainder)._1)}")
    val maxAngle = angle(add(componentToKeep, touchingBorderRemainder), radialDirection)
    //println(s"Is maxAngle in range ? ${math.Pi/4 <= maxAngle && maxAngle <= math.atan(math.sqrt(dimension))}")

    val newVectorAngle = centerToBorderProportion * maxAngle
    //println(s"Is newVectorAngle in range ? ${0 <= newVectorAngle && newVectorAngle <= math.atan(math.sqrt(dimension))}")

    val newRemainderLength = orthogonalRadius * math.tan(newVectorAngle)
    val adjustedRemainder = toLength(remainderToAdjust, newRemainderLength)
    val adjustedVector = add(componentToKeep, adjustedRemainder)

    //println(s"Expected angle : $newVectorAngle, actual angle : ${angle(adjustedVector, radialDirection)}")
    if(adjustedVector.count(_.isNaN) == 0) adjustedVector else vector
  }

}
