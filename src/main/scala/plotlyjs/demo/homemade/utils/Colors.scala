package plotlyjs.demo.homemade.utils

import org.openmole.plotlyjs.{Color => OMColor}
import plotlyjs.demo.homemade.utils.Vectors._

import scala.language.implicitConversions
import scala.math.{abs, random, rint}

object Colors {

  type Color = Seq[Double]

  implicit class ImplicitColor(color: Color) {

    val rgb: Seq[Double] = color.take(3)
    val alpha: Double = if(color.size >= 4) color(3) else 1

    def opacity(alpha: Double): Color = color.take(3) :+ alpha

    def fromHSLtoRGB: Color = {
      val h = color(0)
      val s = color(1)
      val l = color(2)
      val c = (1 - abs(2 * l - 1)) * s
      val h6 = h * 6
      val x = c * (1 - abs(h6 % 2 - 1))
      val rgb1 = h6.toInt match {
        case 0 => Seq(c, x, 0)
        case 1 => Seq(x, c, 0)
        case 2 => Seq(0, c, x)
        case 3 => Seq(0, x, c)
        case 4 => Seq(x, 0, c)
        case 5 => Seq(c, 0, x)
      }
      val m = l - c / 2
      rgb1 + m
    }

    def toOMColor: OMColor = {
      val intRGB = color.rgb.map(c => rint(c * 255).toInt)
      OMColor.rgba(intRGB(0), intRGB(1), intRGB(2), color.alpha)
    }

  }

  def randomColor: Color = (() => random) at 3

  implicit def implicitToOMColor(color: Color): OMColor = color.toOMColor

}
