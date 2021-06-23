package plotlyjs.demo.demo

import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.PlotMode._
import org.openmole.plotlyjs.PlotlyImplicits._
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import plotlyjs.demo.directions.AngularAdjustment.Geometry
import plotlyjs.demo.directions.RegularDirectionsWithLines.{VectorsAndLines, regularDirections}
import plotlyjs.demo.directions._
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

      val layout = Layout.title(title)

      Plotly.plot(plotDiv.ref, plotDataSeq.toJSArray, layout)

      plotDiv
    }

    def scatter3dLinesDataSeq(lineSegments: Seq[(Seq[Double], Seq[Double])], color: Color) = {
      lineSegments.map(line => {
        val coordinates = Seq(line._1, line._2).transpose
        scatter3d
          .x(coordinates(0).toJSArray)
          .y(coordinates(1).toJSArray)
          .z(coordinates(2).toJSArray)
          .setMode(lines)
          .marker(marker
            .size(1)
            .symbol(circle)
            .color(color)
          )._result
      })
    }

    def scatter3dLinesDiv(title: String, cubicLines: Seq[(Seq[Double], Seq[Double])], sphericalLines:  Seq[(Seq[Double], Seq[Double])], color: Color) = {
      val plotDiv = div()

      val plotDataSeq = /*scatter3dLinesDataSeq(cubicLines, color) ++ */scatter3dLinesDataSeq(sphericalLines, color)

      val layout = Layout
        .title(title)
        .showlegend(false)

      Plotly.plot(plotDiv.ref, plotDataSeq.toJSArray, layout)

      plotDiv
    }

    val dimension = 3
    val p = 32
    val points = Data.centeredNCube(dimension, p, hollow = true).filter(_.head >= 0)

    val alphaStep = Math.PI/4 / (p/2.0)

    def cut(regularDirections: VectorsAndLines) = {
      regularDirections.vectors.filterNot(_.read.head >= 0).foreach(_.remove())
      regularDirections.clean
    }
    def cell(regularDirections: VectorsAndLines, sphericalShape: Boolean = false) = {
      regularDirections.vectors.filterNot(_.read.head == +1.0).foreach(_.remove())
      val cleaned = regularDirections.clean
      if(sphericalShape) cleaned.vectors.foreach(_.apply(normalize))
      cleaned
    }
    def toLines(regularDirections: VectorsAndLines) = {
      regularDirections.lineSegments.map { case (v1Ref, v2Ref) => (v1Ref.read, v2Ref.read) }
    }
    lazy val line3dCubicRegularDirections = toLines(cut(regularDirections(dimension, 2 * alphaStep)))
    lazy val line3dSphericalRegularDirections = toLines(cut(regularDirections(dimension, 2 * alphaStep, sphericalShape = true)))
    lazy val line4dCubicRegularDirections = toLines(cell(cut(regularDirections(dimension + 1, 4 * alphaStep))))
    lazy val line4dSphericalRegularDirections = toLines(cell(cut(regularDirections(dimension + 1, 4 * alphaStep)), sphericalShape = true))

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
        RegularDirections.nSphereCovering(dimension, alphaStep, keepCubicShape = true).filter(_.head >= 0),
        RegularDirections.nSphereCovering(dimension, alphaStep).filter(_.head >= 0),
        Color.rgb(0, 0, 0)),
      scatter3dDiv(
        "Building method – 3-sphere cell",
        RegularDirections.nSphereCovering(dimension + 1, 2 * alphaStep, keepCubicShape = true).filter(_.head == +1.0).map(_.tail),
        RegularDirections.nSphereCovering(dimension + 1, 2 * alphaStep, keepCubicShape = true).filter(_.head == +1.0).map(normalize).map(_.tail),
        Color.rgb(0, 0, 0)),
      scatter3dLinesDiv(
        "Building method – 2-sphere",
        line3dCubicRegularDirections,
        line3dSphericalRegularDirections,
        Color.rgb(0, 0, 0)),
      /*
      scatter3dLinesDiv(
        "Building method – 3-sphere cell",
        line4dCubicRegularDirections,
        line4dSphericalRegularDirections,
        Color.rgb(0, 0, 0)),
       */
      //evalDiv,
    )
  }

  val elementDemo: ElementDemo = new ElementDemo {
    def title: String = "AngularAdjustment"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
