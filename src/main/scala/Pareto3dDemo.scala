package plotlyjs.demo

import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import org.openmole.plotlyjs.PlotlyImplicits._

import scala.scalajs.js.JSConverters._
import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.PlotMode.markers
import org.openmole.plotlyjs.ScatterTernaryDataBuilder.ScatterTernaryDataBuilder

import PointSet._

object Pareto3dDemo {

  private val sc = sourcecode.Text {

    val plotDiv = div()

    val shift = 0.1
    val lowCubeCorner = Data.nCube(3, 12).filter(_.contains(0)).map(_.map(_ * 3 + shift))

    val dim = Seq(1, 2, 3)
    val results = Data.dim8Sample100.map(p => Seq(p(dim(0)), p(dim(1)), p(dim(2))))

    val pointSet = new PointSet(lowCubeCorner ++ results)
      .optimizationProblems(Seq(MIN, MIN, MIN))
      .higherPlotIsBetter
      .normalizePlotOutputSpace

    val allData = for(i <- 0 until pointSet.size) yield {
      val rawPoint = pointSet.rawOutputs(i)
      val rawText = rawPoint.indices.map(j => s"o${j+1} : ${rawPoint(j)}").mkString("<br>")
      val plotPoint = pointSet.plotOutputs(i)
      scatterternary
        .a(Seq(plotPoint(0)).toJSArray)
        .b(Seq(plotPoint(1)).toJSArray)
        .c(Seq(plotPoint(2)).toJSArray)
        .set(markers)
        .set(marker
          .color(if(i < lowCubeCorner.size) Color.rgb(0, 0, 0) else Color.rgb(255, 0, 0))
          .symbol(circle)
          .opacity(0.5))
        .hoverinfo("text")
        .text(s"Model output :<br>${rawText}")
        ._result
    }

    val layout = Layout.ternary(
      ternary
        .aaxis(axis.dtick(0.1).title(s"Goal ${dim(0)}"))
        .baxis(axis.dtick(0.1).title(s"Goal ${dim(1)}"))
        .caxis(axis.dtick(0.1).title(s"Goal ${dim(2)}"))
    ).width(800).height(800)
      ._result

    Plotly.newPlot(plotDiv.ref, allData.toJSArray, layout)

    plotDiv
  }

  val elementDemo: ElementDemo = new ElementDemo{
    def title: String = "Pareto 3d"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
