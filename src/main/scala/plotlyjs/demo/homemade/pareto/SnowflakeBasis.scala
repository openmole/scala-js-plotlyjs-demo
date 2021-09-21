package plotlyjs.demo.homemade.pareto

import plotlyjs.demo.homemade.pareto.SnowflakeBasis.cartesianFromPolar
import plotlyjs.demo.homemade.utils.Basis
import plotlyjs.demo.homemade.utils.Vectors._

import scala.math.{atan2, cos, sin}

class SnowflakeBasis(dimension: Int) extends Basis {

  override val size: Int = dimension

  override def basisVector(i: Int): Vector = {
    if (dimension == 2) {
      i match {
        case 0 => cartesianFromPolar(Seq(1, 0))
        case 1 => cartesianFromPolar(Seq(1, 90))
      }
    } else {
      cartesianFromPolar(Seq(1, 360 * i / dimension))
    }
  }

}

object SnowflakeBasis {

  class PolarVector(vector: Vector) extends Vector {
    override def apply(i: Int): Double = vector.apply(i)
    override def length: Int = vector.length
    override def iterator: Iterator[Double] = vector.iterator
    val radius: Double = vector(0)
    val angle: Double = vector(1)
  }

  def polarFromCartesian(vector: Vector): PolarVector = {
    val radius = vector.norm
    val x = vector(0)
    val y = vector(1)
    val angle = atan2(y, x).toDegrees
    new PolarVector(Seq(radius, angle))
  }

  class CartesianVector(vector: Vector) extends Vector {
    override def apply(i: Int): Double = vector.apply(i)
    override def length: Int = vector.length
    override def iterator: Iterator[Double] = vector.iterator
    val x: Double = vector(0)
    val y: Double = vector(1)
  }

  def cartesianFromPolar(vector: Vector): CartesianVector = {
    val radius = vector(0)
    val theta = vector(1).toRadians
    val x = radius * cos(theta)
    val y = radius * sin(theta)
    new CartesianVector(Seq(x, y))
  }

}
