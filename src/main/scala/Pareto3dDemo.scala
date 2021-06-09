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

  val sc = sourcecode.Text {

    val plotDiv = div()

    val dim = Seq(1, 2, 3)
    val results1 = Data.dim8Sample100.map(p => Seq(p(dim(0)), p(dim(1)), p(dim(2))))

    val shift = 0.1
    val results2 = Data.ff
      .filter(p => Math.min(Math.min(p._1, p._2), p._3) == 0)
      .map(p => Seq(p._1 + shift, p._2 + shift, p._3 + shift))

    val pointSet = new PointSet(results1)
      .setOptimizationProblems(Seq(MIN, MIN, MIN))
      .higherPlotIsBetter
      .normalizePlotOutputSpace

    val allData = for(p <- pointSet.getPlotOutputs.map(p => (p(0), p(1), p(2)))) yield scatterternary
      .a(Seq(p._1).toJSArray)
      .b(Seq(p._2).toJSArray)
      .c(Seq(p._3).toJSArray)
      .set(markers).set(marker
        .color(Color.rgb(0, 0, 0))
        .symbol(circle)
        .opacity(0.5)
      )._result

    //TODO show rawOutputs on hovering otherwise the plot is almost useless
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
