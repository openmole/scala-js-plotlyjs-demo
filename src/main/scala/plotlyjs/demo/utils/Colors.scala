package plotlyjs.demo.utils

import org.openmole.plotlyjs.{Color => OMColor}

import plotlyjs.demo.utils.Vectors._

import scala.language.implicitConversions
import scala.math.{abs, rint}

object Colors {

  type Color = Seq[Double]

  implicit class ImplicitColor(color: Color) {

    val alpha: Double = if(color.size >= 4) color(3) else 1

    def withAlpha(alpha: Double): Color = color.take(3) :+ alpha

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

  }

  implicit def implicitToOMColor(color: Color): OMColor = {
    val intColor = color.take(3).map(c => rint(c * 255).toInt)
    OMColor.rgba(intColor(0), intColor(1), intColor(2), color.alpha)
  }

}