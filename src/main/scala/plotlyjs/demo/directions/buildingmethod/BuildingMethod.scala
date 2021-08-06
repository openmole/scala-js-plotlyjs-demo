package plotlyjs.demo.directions.buildingmethod

import plotlyjs.demo.directions.angularadjustment.AngularAdjustment.Splitter._
import plotlyjs.demo.utils.vector.Vectors._

import scala.math._

object BuildingMethod {

  def nSphereCovering(dim: Int, alphaStep: Double, keepCubicShape: Boolean = false): Seq[Vector] = {
    val nSphereDim = dim - 1
    if (nSphereDim == 0) {
      Seq(Seq(-1.0), Seq(+1.0))
    } else {
      val alphaMax = acos(1 / sqrt(dim))

      val cell = (1 to (alphaMax / alphaStep).toInt).flatMap(i => {
        val rOnCell = tan(i * alphaStep)
        val rOnSphere = Seq(rOnCell, 1).normalize.head
        val sphere = nSphereCovering(nSphereDim, alphaStep / rOnSphere).map(_.scale(rOnCell))
        val maxMagnitudes = sphere.map(MaxMagnitudeComponent(_).norm)
        val inside = (sphere zip maxMagnitudes)
          .filter(_._2 <= 1).map(_._1)
        val border = (sphere zip maxMagnitudes)
          .filter(_._2 > 1)
          .map { case (v, m) => (1 / m) * v }
          .filter(_.norm > tan((i - 1) * alphaStep))
        inside ++ border
      }) ++ Seq(Seq.fill(dim - 1)(0.0))

      val cubicNSphere = cell.flatMap(v => {
        (0 to dim).flatMap(insert => {
          val (vLeft, vRight) = v.splitAt(insert)
          Seq(Seq(-1.0), Seq(+1.0)).map(u => {
            vLeft ++ u ++ vRight
          })
        })
      }) ++ (0 until pow(2, dim).toInt).map(_.toBinaryString.toInt).map(s"%0${dim}d".format(_).map(c => if (c == '0') -1.0 else +1.0))

      if (keepCubicShape) {
        cubicNSphere
      } else {
        cubicNSphere.map(_.normalize)
      }
    }
  }

  def mainTest(args: Array[String]): Unit = {
    for (dim <- 1 to 42) {
      println(dim, nSphereCovering(dim, Pi / 4 / 4).size)
    }
  }

}
