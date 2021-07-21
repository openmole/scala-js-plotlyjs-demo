package plotlyjs.demo.demo

import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.Plotly
import org.openmole.plotlyjs.all._
import plotlyjs.demo.directions.restrictedspacetransformation.v4.IndexVectors._
import plotlyjs.demo.utils.Colors._
import plotlyjs.demo.utils.PointSet.MIN
import plotlyjs.demo.utils.Vectors.{ImplicitScalar, ImplicitVector}
import plotlyjs.demo.utils.{Data, PointSet}

import scala.scalajs.js.JSConverters.JSRichIterableOnce

object ParallelCoordinatesDemo {

  lazy private val sc = sourcecode.Text {

    val plotDiv = div()

    val dim = 3
    val p = 4
    val results = Data.lowCorner(dim, p)//Data.dim8Sample100.map(_.take(dim))
    val pointSet = new PointSet(results)
      .optimizationProblems(MIN at dim)
      .lowerPlotIsBetter

    val groupedResults = pointSet.spaceNormalizedOutputs.groupBy[IndexVector](_.orthogonalComponent(1 at dim))
    val keys = groupedResults.keys.toSeq

    val plotData = parallelCoordinates
      .line(line
        .color(keys.zipWithIndex.flatMap { case (k, i) => groupedResults(k).map(_ => i) })
        .colorscale(keys.indices.map(i => (i, Seq(1.0, 0.0, 0.0)/*Colors.randomColor*/))) // not taken in account
      )
      .dimensions(keys.flatMap(k => groupedResults(k)).transpose.map(values => dimension.values(values.toJSArray)._result).toJSArray)
      ._result

    Plotly.newPlot(plotDiv.ref, Seq(plotData).toJSArray)

    plotDiv
  }

  val elementDemo: Demo = new Demo{
    def title: String = "Parallel Coordinates"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
