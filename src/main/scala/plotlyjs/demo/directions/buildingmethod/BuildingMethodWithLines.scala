package plotlyjs.demo.directions.buildingmethod

import plotlyjs.demo.directions.angularadjustment.AngularAdjustment.Splitter._
import plotlyjs.demo.utils.Graph
import plotlyjs.demo.utils.Graph._
import plotlyjs.demo.utils.Vectors._

import scala.math._

object BuildingMethodWithLines {

  def nSphereCovering(dim: Int, alphaStep: Double, keepCubicShape: Boolean = false): Graph[Vector] = {
    val nSphereDim = dim - 1
    if (nSphereDim == 0) {
      Graph(Seq(-1.0), Seq(+1.0))
    } else {
      val alphaMax = acos(1 / sqrt(dim))

      val cell = {
        var graph = Graph(Seq.fill(nSphereDim)(0.0))
        var prevSphere = graph;
        (1 to (alphaMax / alphaStep).toInt).foreach(i => {
          val rOnCell = tan(i * alphaStep)
          val rOnSphere = Seq(rOnCell, 1).normalize.head
          val sphere = nSphereCovering(nSphereDim, alphaStep / rOnSphere).mapGraph(scale(rOnCell))
          if (nSphereDim == 1) {
            graph = graph ++ sphere
            sphere.vertices.foreach(vertex => {
              if (prevSphere.vertices.size == 1) {
                graph = graph.added(prevSphere.vertices.head --> vertex)
              } else {
                prevSphere.vertices.foreach(prevVertex => {
                  if (signum(prevVertex(0)) == signum(vertex(0))) {
                    graph = graph.added(prevVertex --> vertex)
                  }
                })
              }
            })
            prevSphere = sphere
          } else {
            val maxMagnitude = sphere.mapVertices(MaxMagnitudeComponent(_).norm)
            val inside = sphere
              .filter(maxMagnitude(_) <= 1)
            val border = sphere
              .filter(maxMagnitude(_) > 1)
              .mapGraph(v => (1 / maxMagnitude(v)) * v)
              .filter(_.norm > tan((i - 1) * alphaStep))
            graph = graph ++ inside ++ border
          }
        })
        graph
      }

      val cubicNSphere = (0 to dim).map(insert => {
        Seq(Seq(-1.0), Seq(+1.0)).map(u => {
          cell.mapGraph(v => {
            val (vLeft, vRight) = v.splitAt(insert)
            vLeft ++ u ++ vRight
          })
        }).reduce(_ ++ _)
      }).reduce(_ ++ _) ++ Graph.fromVertices((0 until pow(2, dim).toInt).map(_.toBinaryString.toInt).map(s"%0${dim}d".format(_).map(c => if (c == '0') -1.0 else +1.0)).toSet[Vector])

      if (keepCubicShape) {
        cubicNSphere
      } else {
        cubicNSphere.mapGraph(normalize)
      }
    }
  }

  def mainTest(args: Array[String]): Unit = {
    println(nSphereCovering(3, Pi / 4 / 4).arrows)
  }

}
