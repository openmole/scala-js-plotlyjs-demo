package plotlyjs.demo.demo

import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.PlotMode.markers
import org.openmole.plotlyjs.PlotlyImplicits._
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import plotlyjs.demo.directions.AngularAdjustment.Geometry
import plotlyjs.demo.directions.{AngularAdjustment, CubicAngularAdjustment, DiscAngularSpaceAdjustment}
import plotlyjs.demo.utils.Data
import plotlyjs.demo.utils.Vectors.scale

import scala.scalajs.js.JSConverters.JSRichIterableOnce

object AngularAdjustmentTest {

  private val sc = sourcecode.Text {

    def scatter3dData(name: String, points: Seq[Seq[Double]], color: Color) = {
      val pointsT = points.transpose
      scatter3d
        .name(name)
        .x(pointsT(0).toJSArray)
        .y(pointsT(1).toJSArray)
        .z(pointsT(2).toJSArray)
        .setMode(markers)
        .marker(marker
          .size(1)
          .symbol(circle)
          .color(color)
        )._result
    }

    def scatter3dDiv(cubicPoints: Seq[Seq[Double]], sphericalPoints: Seq[Seq[Double]], color: Color) = {
      val plotDiv = div()

      val plotDataSeq = Seq(
        scatter3dData("cubic", cubicPoints, color),
        scatter3dData("spherical", sphericalPoints, color),
      )

      Plotly.plot(plotDiv.ref, plotDataSeq.toJSArray)

      plotDiv
    }

    val highCorner = Data.highCorner(3, 16).map(scale(2))

    div(
      scatter3dDiv(
        highCorner,
        highCorner.map(AngularAdjustment.spacialAdjustedNormalization(Geometry.cubic, _)),
        Color.rgb(255, 0, 0)),
      scatter3dDiv(
        highCorner.map(AngularAdjustment.cellRadialAdjustment(Geometry.cubic, _)),
        highCorner.map(AngularAdjustment.cellRadialAdjustment(Geometry.cubic, _)).map(AngularAdjustment.spacialAdjustedNormalization(Geometry.cubic, _)),
        Color.rgb(0, 0, 255)),
      /*
      scatter3dDiv(
        highCorner.map(DiscAngularSpaceAdjustment.angularAdjustment).filter(_ != null),
        highCorner.map(DiscAngularSpaceAdjustment.angularAdjustment).filter(_ != null).map(AngularAdjustment.spacialAdjustedNormalization(Geometry.cubic, _)),
        Color.rgb(0, 255, 0)),
      */
      scatter3dDiv(
        highCorner.map(CubicAngularAdjustment.angularAdjustment).filter(_ != null),
        highCorner.map(CubicAngularAdjustment.angularAdjustment).filter(_ != null).map(AngularAdjustment.spacialAdjustedNormalization(Geometry.cubic, _)),
        Color.rgb(0, 255, 0)),
    )
  }

  val elementDemo: ElementDemo = new ElementDemo {
    def title: String = "AngularAdjustment"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
