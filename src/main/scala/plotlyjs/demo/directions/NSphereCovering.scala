package plotlyjs.demo.directions

import scala.math._
import plotlyjs.demo.utils.Vectors._

object NSphereCovering {

  def nSphereCovering(n: Int, alphaStep: Double, keepCubicShape: Boolean = false): Seq[Seq[Double]] = {
    val dim = n + 1
    if(n == 0) {
      Seq(Seq(-1.0), Seq(+1.0))
    } else {
      val alphaMax = acos(1/sqrt(dim))

      val cellVectors = (1 to (alphaMax / alphaStep).toInt).flatMap(i => {
          val rOnCell = tan(i * alphaStep)
          val rOnSphere = Seq(rOnCell, 1).normalize.head
          nSphereCovering(n - 1, alphaStep / rOnSphere).map(scale(rOnCell))
      }).filter(_.map(abs).max <= 1) ++ Seq(Seq.fill(dim - 1)(0.0))

      val cubicNSphere = cellVectors.flatMap(v => {
        (0 to dimension(v)).flatMap(number => {
          val (v1, v2) = v.splitAt(number)
          Seq(Seq(-1.0), Seq(+1.0)).map(u => {
            v1 ++ u ++ v2
          })
        })
      }) ++ (0 until pow(2, dim).toInt).map(_.toBinaryString.toInt).map(s"%0${dim}d".format(_).map(c => if(c == '0') -1.0 else +1.0))

      if(keepCubicShape) {
        cubicNSphere
      } else {
        cubicNSphere.map(normalize)
      }
    }
  }

}
