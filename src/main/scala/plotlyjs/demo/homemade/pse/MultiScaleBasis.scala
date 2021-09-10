package plotlyjs.demo.homemade.pse

import plotlyjs.demo.utils.Basis
import plotlyjs.demo.utils.vector.Vectors._

import scala.math.pow

case class MultiScaleBasis(sourceDimension: Int, subdivisions: Seq[Int], destinationDimension: Int, allowStretch: Boolean = false, gap: Int = 1) extends Basis {

  val remainder: Int = sourceDimension % destinationDimension
  val stretchable: Boolean = remainder != 0
  val stretched: Boolean = allowStretch && stretchable

  def axisIndex(i: Int): Int = {
    i % destinationDimension
  }

  def scaleIndex(i: Int): Int = {
    i / destinationDimension
  }

  def axis(i: Int): Int = {
    axisIndex(i)
  }

  def scale(i: Int): Double = {
    pow(subdivisions(i) + gap, scaleIndex(i))
  }

  val maxScaleIndex: Int = scaleIndex(sourceDimension - 1)

  override val size: Int = sourceDimension

  override def basisVector(i: Int): Vector = {
    (0.0 at destinationDimension).replace(axis(i), 1.0) * scale(i)
  }

  override def transform(vector: Vector): Vector = {
    if ((destinationDimension until sourceDimension).map(vector(_).isWhole).reduceOption(_ && _).getOrElse(true)) {
      super.transform(vector)
    } else {
      throw new IllegalArgumentException(s"Coordinates from index $destinationDimension until $sourceDimension must be whole.")
    }
  }

  def totalSize(i: Int): Double = {
    val maxParallel = sourceDimension - 1 - ((sourceDimension - 1 - i) % destinationDimension)
    (basisVector(maxParallel) * subdivisions(i)).norm
  }

  def size(destinationAxis: Int): Double = {
    for (i <- 0 until sourceDimension) {
      if (axis(i) == destinationAxis) {
        return totalSize(i)
      }
    }
    -1
  }

}
