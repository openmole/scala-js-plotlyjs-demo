package plotlyjs.demo.directions

import plotlyjs.demo.directions.AngularAdjustment.Splitter._
import plotlyjs.demo.utils.Vectors._
import plotlyjs.demo.utils.Graph

import scala.math._

object RegularDirectionsWithLines2 {

  def nSphereCovering(dim: Int, alphaStep: Double, keepCubicShape: Boolean = false): Graph[Vector] = {
    val nSphereDim = dim - 1
    if(nSphereDim == 0) {
      new Graph(Set(Seq(-1.0), Seq(+1.0)))
    } else {
      val alphaMax = acos(1/sqrt(dim))

      val cellVectors = if(alphaStep < alphaMax) {
        (1 to (alphaMax / alphaStep).toInt).map(i => {
          val rOnCell = tan(i * alphaStep)
          val rOnSphere = Seq(rOnCell, 1).normalize.head
          val sphere = nSphereCovering(nSphereDim, alphaStep / rOnSphere).mapGraph(scale(rOnCell))
          val maxMagnitude = sphere.mapVertices(maxMagnitudeComponent(_).norm)
          val inside = sphere
            .filter(maxMagnitude(_) <= 1)
          val border = sphere
            .filter(maxMagnitude(_) > 1)
            .mapGraph(v => (1/maxMagnitude(v)) *: v)
            .filter(_.norm > tan((i - 1) * alphaStep))
          inside ++ border
        }).reduce(_ ++ _)
      } else {
        new Graph(Set[Vector]())
      } ++ new Graph(Set(Seq.fill(dim - 1)(0.0)))

      val cubicNSphere = (0 to dim).map(insert => {
        Seq(Seq(-1.0), Seq(+1.0)).map(u => {
          cellVectors.mapGraph(v => {
            val (vLeft, vRight) = v.splitAt(insert)
            vLeft ++ u ++ vRight
          })
        }).reduce(_ ++ _)
      }).reduce(_ ++ _) ++ new Graph((0 until pow(2, dim).toInt).map(_.toBinaryString.toInt).map(s"%0${dim}d".format(_).map(c => if (c == '0') -1.0 else +1.0)).toSet[Vector])

      if(keepCubicShape) {
        cubicNSphere
      } else {
        cubicNSphere.mapGraph(normalize)
      }
    }
  }

  def mainTest(args: Array[String]): Unit = {
    println(nSphereCovering(3, Pi/4 / 4).hashMap)
  }

}
