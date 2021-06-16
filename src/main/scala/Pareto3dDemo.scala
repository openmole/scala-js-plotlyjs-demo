package plotlyjs.demo

import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import org.openmole.plotlyjs.PlotlyImplicits._

import scala.scalajs.js.JSConverters._
import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.PlotMode.markers
import org.openmole.plotlyjs.ScatterTernaryDataBuilder.ScatterTernaryDataBuilder
import tools.PointSet
import tools.PointSet._

object Pareto3dDemo {

  private val sc = sourcecode.Text {

    val plotDiv = div()

    val lowCorner = Data.lowCorner(3, 8).map(_.map(_ * 3))

    val dim = Seq(1, 2, 3)
    val results = Data.dim8Sample100.map(p => Seq(p(dim(0)), p(dim(1)), p(dim(2))))

    val pointSet = new PointSet(lowCorner ++ results)
      .optimizationProblems(Seq(MIN, MIN, MIN))
      .higherPlotIsBetter

    def scatterTernaryData(name: String, from: Int, until: Int, color: Color): PlotData = {
      val rawOutputs = pointSet.rawOutputs.slice(from, until)
      val spaceNormalizedOutputs = pointSet.spaceNormalizedOutputs.slice(from, until);
      scatterternary
        .name(name)
        .a(spaceNormalizedOutputs.map(_(0)).toJSArray)
        .b(spaceNormalizedOutputs.map(_(1)).toJSArray)
        .c(spaceNormalizedOutputs.map(_(2)).toJSArray)
        .set(markers)
        .set(marker
          .color(color)
          .symbol(circle)
          .opacity(0.5))
        .hoverinfo("text")
        .text(rawOutputs.map(p => s"Model output :<br>${
          (p.zipWithIndex map { case (c, i) => s"o${i+1} : $c" }).mkString("<br>")
        }").toJSArray)
        ._result
    }

    val lowCornerData = scatterTernaryData(
      "Low corner",
      0, lowCorner.size,
      Color.rgb(0, 0, 0))
    val resultsData = scatterTernaryData(
      "Results",
      lowCorner.size, pointSet.size,
      Color.rgb(255, 0, 0))

    val layout = Layout.ternary(
      ternary
        .aaxis(axis.dtick(0.1).title(s"Goal ${dim(0)}"))
        .baxis(axis.dtick(0.1).title(s"Goal ${dim(1)}"))
        .caxis(axis.dtick(0.1).title(s"Goal ${dim(2)}"))
    ).width(800).height(800)
      ._result

    Plotly.newPlot(plotDiv.ref, Seq(lowCornerData, resultsData).toJSArray, layout)

    plotDiv
  }

  val elementDemo: ElementDemo = new ElementDemo{
    def title: String = "Pareto 3d"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
