package plotlyjs.demo.homemade.pareto

import plotlyjs.demo.homemade.pareto.SnowflakeBasis.cartesianFromPolar
import plotlyjs.demo.utils.Basis
import plotlyjs.demo.utils.vector.Vectors._

import scala.math.{atan2, cos, exp, sin}

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
    val r = vector.norm
    val x = vector(0)
    val y = vector(1)
    val theta = atan2(y, x).toDegrees
    new PolarVector(Seq(r, theta))
  }

  class CartesianVector(vector: Vector) extends Vector {
    override def apply(i: Int): Double = vector.apply(i)
    override def length: Int = vector.length
    override def iterator: Iterator[Double] = vector.iterator
    val x: Double = vector(0)
    val y: Double = vector(1)
  }

  def cartesianFromPolar(vector: Vector): CartesianVector = {
    val r = vector(0)
    val theta = vector(1).toRadians
    val x = r * cos(theta)
    val y = r * sin(theta)
    new CartesianVector(Seq(x, y))
  }

}
