package plotlyjs.demo.demo

import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.PlotMode.markers
import org.openmole.plotlyjs.PlotlyImplicits._
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import plotlyjs.demo.directions.AngularAdjustment.Geometry
import plotlyjs.demo.directions.{AngularAdjustment, CubicAngularAdjustment, NSphereCovering}
import plotlyjs.demo.utils.Data
import plotlyjs.demo.utils.Vectors._

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

    def scatter3dDiv(title: String, cubicPoints: Seq[Seq[Double]], sphericalPoints: Seq[Seq[Double]], color: Color) = {
      val plotDiv = div()

      val plotDataSeq = Seq(
        scatter3dData("cubic", cubicPoints, color),
        scatter3dData("spherical", sphericalPoints, color),
      )

      val layout = Layout
        .title(title)

      Plotly.plot(plotDiv.ref, plotDataSeq.toJSArray, layout)

      plotDiv
    }

    val dimension = 3
    val step = 8
    val points = Data.centeredNCube(dimension, 2 * step, hollow = true).filter(_.head >= 0)

    val alphaStep = Math.PI/4 / step

    div(
      scatter3dDiv(
        "Cube – no adjustment",
        points,
        points.map(normalize),
        Color.rgb(255, 0, 0)),
      scatter3dDiv(
        "Radial angular adjustment",
        points.map(AngularAdjustment.cellRadialAdjustment(Geometry.cubic, _)),
        points.map(AngularAdjustment.cellRadialAdjustment(Geometry.cubic, _)).map(normalize),
        Color.rgb(0, 0, 255)),
      /*
      scatter3dDiv(
        points.map(DiscAngularSpaceAdjustment.angularAdjustment).filter(_ != null),
        points.map(DiscAngularSpaceAdjustment.angularAdjustment).filter(_ != null).map(AngularAdjustment.spacialAdjustedNormalization(Geometry.cubic, _)),
        Color.rgb(0, 255, 0)),
      */
      scatter3dDiv(
        "Cartesian angular adjustment",
        points.map(CubicAngularAdjustment.angularAdjustment).filter(_ != null),
        points.map(CubicAngularAdjustment.angularAdjustment).filter(_ != null).map(normalize),
        Color.rgb(0, 255, 0)),
      scatter3dDiv(
        "Building method – 2-sphere",
        NSphereCovering.nSphereCovering(dimension - 1, alphaStep, keepCubicShape = true).filter(_.head >= 0),
        NSphereCovering.nSphereCovering(dimension - 1, alphaStep).filter(_.head >= 0),
        Color.rgb(0, 0, 0)),
      scatter3dDiv(
        "Building method – 3-sphere cell",
        NSphereCovering.nSphereCovering(dimension, alphaStep, keepCubicShape = true).filter(_.head == +1.0).map(_.tail),
        NSphereCovering.nSphereCovering(dimension, alphaStep, keepCubicShape = true).filter(_.head == +1.0).map(normalize).map(_.tail),
        Color.rgb(0, 0, 0)),
      //evalDiv,
    )
  }

  val elementDemo: ElementDemo = new ElementDemo {
    def title: String = "AngularAdjustment"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
