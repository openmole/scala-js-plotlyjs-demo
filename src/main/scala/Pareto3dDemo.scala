package plotlyjs.demo

import PointSet._

import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import org.openmole.plotlyjs.PlotlyImplicits._

import scala.scalajs.js.JSConverters._
import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.PlotMode.markers
import org.openmole.plotlyjs.ScatterTernaryDataBuilder.ScatterTernaryDataBuilder

object Pareto3dDemo {

  val sc = sourcecode.Text {

    val plotDiv = div()

    val dim = Seq(1, 2, 3)
    //val results = Data.dim8Sample100.map(p => (p(dim(0)), p(dim(1)), p(dim(2))))
    val results = Data.ff.filter(p => Math.min(Math.min(p._1, p._2), p._3) == 0)
    //TODO deal with negative values
    val transformedResults = new PointSet(results.map(p => Seq(p._1, p._2, p._3)))
      .setOptimizationProblems(Seq(MAX, MAX, MAX))
      .higherPlotIsBetter
      .normalizePlotOutputSpace
      .getPlotOutputs
      .map(p => (p(0), p(1), p(2)))

    val enhancedResults = transformedResults.map(p => (p._1, p._2, p._3, p._1 + p._2 + p._3))
    var maxNormL1 = 0.0
    for(p <- enhancedResults) maxNormL1 = Math.max(maxNormL1, p._4)

    val allData = for(p <- enhancedResults) yield scatterternary
      .a(Seq(p._1).toJSArray)
      .b(Seq(p._2).toJSArray)
      .c(Seq(p._3).toJSArray)
      .set(markers).set(marker
        .size(4 + 16*(maxNormL1 - p._4)/maxNormL1)
        .color(Color.rgb(0, 0, 0))
        .symbol(circle)
        .opacity(0.5)
      )._result

    val layout = Layout.ternary(
      ternary
        .aaxis(axis.dtick(0.1).title(s"dim ${dim(0)}"))
        .baxis(axis.dtick(0.1).title(s"dim ${dim(1)}"))
        .caxis(axis.dtick(0.1).title(s"dim ${dim(2)}"))
    ).width(800).height(800)
      ._result

    Plotly.newPlot(plotDiv.ref, allData.toJSArray, layout)

    plotDiv
  }

  val elementDemo = new ElementDemo{
    def title: String = "Pareto3d"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
