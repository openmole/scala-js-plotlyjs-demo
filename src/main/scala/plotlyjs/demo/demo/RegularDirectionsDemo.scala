package plotlyjs.demo.demo

import com.raquo.laminar.api.L.{HtmlElement, div}
import org.openmole.plotlyjs.PlotMode._
import org.openmole.plotlyjs.PlotlyImplicits._
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import plotlyjs.demo.directions.angularadjustment.AngularAdjustment.Geometry
import plotlyjs.demo.directions.angularadjustment.{AngularAdjustment, CubicAngularAdjustment}
import plotlyjs.demo.directions.buildingmethod.{BuildingMethod, BuildingMethodWithCache, BuildingMethodWithLines}
import plotlyjs.demo.directions.restrictedspacetransformation.v4._
import plotlyjs.demo.directions.restrictedspacetransformation.v4.IndexVectors._
import plotlyjs.demo.utils.Data
import plotlyjs.demo.utils.Utils.onDemand
import plotlyjs.demo.utils.Vectors._

import scala.scalajs.js.JSConverters.JSRichIterableOnce
import scala.util.control.Breaks.breakable

object RegularDirectionsDemo {

  lazy private val sc = sourcecode.Text {

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

    def scatter3dDiv(title: String, cubicPoints: Seq[Seq[Double]], sphericalPoints: Seq[Seq[Double]]) = {
      val plotDiv = div()

      val plotDataSeq = Seq(
        scatter3dData("cubic", cubicPoints, Color.rgba(204, 0, 0, 1)),
        scatter3dData("spherical", sphericalPoints, Color.rgba(0, 123, 255, 1)),
      )

      val layout = Layout.title(title)

      Plotly.plot(plotDiv.ref, plotDataSeq.toJSArray, layout)

      plotDiv
    }

    def polylines(lineSegments: Seq[(Seq[Double], Seq[Double])]) = {
      type Line = Seq[Seq[Double]]
      var lines: Seq[Line] = lineSegments.map { case (v1, v2) => Seq(v1, v2) }
      println(lines.size)
      var done = false

      done = true //deactivated

      while(!done) {
        done = true
        var newLines = Seq[Line]()
        var oldLines = Seq[Line]()
        breakable {
          for(i1 <- 0 until lines.size - 1) {
            val l1 = lines(i1)
            for(i2 <- i1 + 1 until lines.size) {
              val l2 = lines(i2)

              Seq((l1, l2), (l1.reverse, l2), (l1, l2.reverse), (l1.reverse, l2.reverse)).foreach { case (lLast, lFirst) =>
                if(lLast.last == lFirst.head) {
                  val lLeft = lLast
                  val lRight = lFirst.drop(1)
                  if(lLeft.intersect(lRight).isEmpty) {
                    newLines = newLines :+ (lLeft ++ lRight)
                    oldLines = oldLines ++ Seq(l1, l2)
                    done = false
                    //break
                  }
                }
              }
            }
          }
        }
        lines = lines ++ newLines
        lines = lines.filterNot(oldLines.contains)
        println(lines.size)
      }
      lines
    }

    def scatter3dLinesDataSeq(lineSegments: Seq[(Seq[Double], Seq[Double])], color: Color) = {
      val seqLines: Seq[Seq[Seq[Double]]] = lineSegments.map { case (v1, v2) => Seq(v1, v2) }
      /*polylines(lineSegments)*/seqLines.map(polyline => {
        val coordinates = polyline.transpose
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

    def scatter3dLinesDiv(title: String, sphericalLines:  Seq[(Seq[Double], Seq[Double])], color: Color) = {
      val plotDiv = div()

      val plotDataSeq = scatter3dLinesDataSeq(sphericalLines, color)

      val layout = Layout
        .title(title)
        .showlegend(false)

      Plotly.plot(plotDiv.ref, plotDataSeq.toJSArray, layout)

      plotDiv
    }

    val dimension = 3
    val p = 31
    lazy val points = Data.centeredNCube(dimension, p, hollow = true).filter(_.head >= 0)

    val alphaStep = Math.PI/4 / (p/2.0)
    val linesAlphaStep = 2 * alphaStep

    lazy val sphereRST = Transformation.fromSquareToCircle(Data.centeredNCube(dimension, 2 * p + 1, hollow = true))
    lazy val cubeWithLineRST = {
      val radius = 8
      Data
        .integerNCube(dimension, 2 * radius + 1, hollow = true)
        .map[IndexVector](sub(radius.toDouble)(_))
        .flatMap(IndexedTransformation.fromIndexToCircle)
        .map(IndexedTransformation.fromCircleToIndex)
        .map(indexVector => (indexVector, IndexedTransformation.neighbourhood(indexVector)))

        .map { case (indexVector, neighbourhood) =>
          println(neighbourhood)
          (indexVector, neighbourhood)
        }

        .flatMap {
          case (indexVector, neighbourhood) if neighbourhood.nonEmpty => neighbourhood.map(neighbour => (indexVector.vector, neighbour.vector))
          case _ => None
        }
    }

    div(
      onDemand("Cube – no adjustment", title => scatter3dDiv(
        title,
        points,
        points.map(normalize)
      )),
      onDemand("Radial angular adjustment", title => scatter3dDiv(
        title,
        points.map(AngularAdjustment.cellRadialAdjustment(Geometry.cubic, _)),
        points.map(AngularAdjustment.cellRadialAdjustment(Geometry.cubic, _)).map(normalize)
      )),
      onDemand("Cartesian angular adjustment", title => scatter3dDiv(
        title,
        points.map(CubicAngularAdjustment.angularAdjustment).filter(_ != null),
        points.map(CubicAngularAdjustment.angularAdjustment).filter(_ != null).map(normalize)
      )),
      onDemand("Building method – 2-sphere", title => scatter3dDiv(
        title,
        BuildingMethod.nSphereCovering(dimension, alphaStep, keepCubicShape = true).filter(_.head >= 0),
        BuildingMethod.nSphereCovering(dimension, alphaStep).filter(_.head >= 0)
      )),
      onDemand("Building method – 3-sphere cell", title => scatter3dDiv(
        title,
        BuildingMethod.nSphereCovering(dimension + 1, 2 * alphaStep, keepCubicShape = true).filter(_.head == +1.0).map(_.tail),
        BuildingMethod.nSphereCovering(dimension + 1, 2 * alphaStep, keepCubicShape = true).filter(_.head == +1.0).map(normalize).map(_.tail)
      )),
      onDemand("Building method with lines – 2-sphere", title => scatter3dLinesDiv(
        title,
        BuildingMethodWithLines.nSphereCovering(dimension, linesAlphaStep).arrows.filter { case (v1, v2) => v1.head >= 0 && v2.head >= 0 }.toSeq,
        Color.rgb(0, 0, 0)
      )),
      onDemand("Building method with lines – 3-sphere cell", title => scatter3dLinesDiv(
        title,
        (BuildingMethodWithLines.nSphereCovering(dimension + 1, linesAlphaStep, keepCubicShape = true).arrows
          .filter { case (v1, v2) => v1.head == +1.0 && v2.head == +1.0 } map { case (v1, v2) => (v1.normalize.tail, v2.normalize.tail) }).toSeq,
        Color.rgb(0, 0, 0)
      )),
      onDemand("Cached building method", title => scatter3dDiv(
        title,
        Seq(Seq(0.0, 0.0, 0.0)),
        BuildingMethodWithCache.nSphereCovering(dimension, alphaStep, 0)
      )),
      onDemand("Restricted space transformation – 2-sphere", title => scatter3dDiv(
        title,
        sphereRST.map(Transformation.fromCircleToSquare).filter(_.head >= 0),
        sphereRST.filter(_.head >= 0)
      )),
      onDemand("RST with transformation lines – 2-dimensional", title => scatter3dLinesDiv(
        title,
        Transformation.fromSquareToCircle(Data.centeredNCube(dimension, p/2, hollow = true)).map(circleVector => (circleVector, Transformation.fromCircleToSquare(circleVector))).filter(_._1.head >= 0),
        Color.rgb(0, 0, 0)
      )),
      onDemand("Restricted space transformation with lines – 2-cube", title => scatter3dLinesDiv(
        title,
        cubeWithLineRST.filter(_._1.head >= 0),
        Color.rgb(0, 0, 0)
      )),
      onDemand("Restricted space transformation with lines – 2-sphere", title => scatter3dLinesDiv(
        title,
        cubeWithLineRST.map(line => (IndexedTransformation.fromIndexToCircle(line._1).get, IndexedTransformation.fromIndexToCircle(line._2).get)).filter(_._1.head >= 0),
        Color.rgb(0, 0, 0)
      )),
    )
  }

  val elementDemo: ElementDemo = new ElementDemo {
    def title: String = "Regular directions"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

  def mainTest(args: Array[String]): Unit = {
    println(RegularDirectionsDemo.elementDemo)
  }

}
