package plotlyjs.demo.demo

import com.raquo.laminar.api.L.{HtmlElement, div}
import org.openmole.plotlyjs.HistNorm.{percent, probability}
import org.openmole.plotlyjs.HistogramDataBuilder.HistogramDataBuilder
import org.openmole.plotlyjs.PlotMode._
import org.openmole.plotlyjs.PlotlyImplicits._
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import plotlyjs.demo.directions.angularadjustment.AngularAdjustment.Geometry
import plotlyjs.demo.directions.angularadjustment.{AngularAdjustment, CubicAngularAdjustment}
import plotlyjs.demo.directions.buildingmethod.{BuildingMethod, BuildingMethodWithCache, BuildingMethodWithLines}
import plotlyjs.demo.directions.restrictedspacetransformation.v4.Evaluation._
import plotlyjs.demo.directions.restrictedspacetransformation.v4.{Evaluation, IndexedTransformation, MaxMagnitude, Transformation}
import plotlyjs.demo.utils.vector.IntVectors._
import plotlyjs.demo.utils.{Data, ParetoFront}
import plotlyjs.demo.utils.Utils.{onDemand, reloadOnDemand}
import plotlyjs.demo.utils.vector.Vectors._

import scala.scalajs.js
import scala.scalajs.js.JSConverters.JSRichIterableOnce
import scala.util.control.Breaks.breakable

object RegularDirectionsDemo {

  private lazy val sc = sourcecode.Text {

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

    val radius = p/2
    lazy val cubeWithLineRST = {
      val radius = 8
      Data
        .integerNCube(dimension, 2 * radius + 1, hollow = true)
        .map[IntVector](_.vector.sub(radius.toDouble))
        .flatMap(IndexedTransformation.fromIndexToCircle)
        .map(IndexedTransformation.fromCircleToIndex)
        .map(indexVector => (indexVector, IndexedTransformation.neighbourhood(indexVector)))
        .flatMap {
          case (indexVector, neighbourhood) if neighbourhood.nonEmpty => neighbourhood.map(neighbour => (indexVector.vector, neighbour.vector))
          case _ => None
        }
    }

    div(
      onDemand("Cube – no adjustment", title => scatter3dDiv(
        title,
        points,
        points.map(_.normalize)
      )),
      onDemand("Radial angular adjustment", title => scatter3dDiv(
        title,
        points.map(AngularAdjustment.cellRadialAdjustment(Geometry.cubic, _)),
        points.map(AngularAdjustment.cellRadialAdjustment(Geometry.cubic, _)).map(_.normalize)
      )),
      onDemand("Cartesian angular adjustment", title => scatter3dDiv(
        title,
        points.map(CubicAngularAdjustment.angularAdjustment).filter(_ != null),
        points.map(CubicAngularAdjustment.angularAdjustment).filter(_ != null).map(_.normalize)
      )),
      onDemand("Building method – 2-sphere", title => scatter3dDiv(
        title,
        BuildingMethod.nSphereCovering(dimension, alphaStep, keepCubicShape = true).filter(_.head >= 0),
        BuildingMethod.nSphereCovering(dimension, alphaStep).filter(_.head >= 0)
      )),
      onDemand("Building method – 3-sphere cell", title => scatter3dDiv(
        title,
        BuildingMethod.nSphereCovering(dimension + 1, 2 * alphaStep, keepCubicShape = true).filter(_.head == +1.0).map(_.tail),
        BuildingMethod.nSphereCovering(dimension + 1, 2 * alphaStep, keepCubicShape = true).filter(_.head == +1.0).map(_.normalize).map(_.tail)
      )),
      onDemand("Building method with lines – 2-sphere", title => scatter3dLinesDiv(
        title,
        BuildingMethodWithLines.nSphereCovering(dimension, linesAlphaStep).arrows.filter(arrow => arrow.tail.head >= 0 && arrow.head.head >= 0).map(arrow => (arrow.tail, arrow.head)).toSeq,
        Color.rgb(0, 0, 0)
      )),
      onDemand("Building method with lines – 3-sphere face", title => scatter3dLinesDiv(
        title,
        BuildingMethodWithLines.nSphereCovering(dimension + 1, linesAlphaStep, keepCubicShape = true).arrows
          .filter(arrow => arrow.tail.head == +1.0 && arrow.head.head == +1.0).map(arrow => (arrow.tail.normalize.tail, arrow.head.normalize.tail) ).toSeq,
        Color.rgb(0, 0, 0)
      )),
      onDemand("Cached building method", title => scatter3dDiv(
        title,
        Seq(Seq(0.0, 0.0, 0.0)),
        BuildingMethodWithCache.nSphereCovering(dimension, alphaStep, 0)
      )),
      onDemand("Restricted space transformation – index", title => scatter3dDiv(
        title,
        IndexedTransformation.centeredNCubeSurface(3, 8).map(_.vector).toSeq,
        Seq(0.0 at 3)
      )),
      onDemand("Restricted space transformation – 2-sphere", title => {
        val sphere = IndexedTransformation.circle(3, 16)
        scatter3dDiv(
          title,
          sphere.map[Vector](IndexedTransformation.fromCircleToIndex).filter(_.head >= 0).toSeq,
          sphere.filter(_.head >= 0).toSeq
        )
      }),
      onDemand("Restricted space transformation – 3-sphere face", title => {
        val radius = 8
        val _3_sphere = IndexedTransformation
          .circleWithIndex(4, radius)
          //.filter(_._2.head == radius)
          .map[(Vector, Vector)] { case (circleVector, indexVector) => (circleVector.tail, indexVector.tail) }
        scatter3dDiv(
          title,
          _3_sphere.map(_._2).toSeq,
          _3_sphere.map(_._1).toSeq
        )
      }),
      onDemand("RST with transformation lines", title => scatter3dLinesDiv(
        title,
        IndexedTransformation.circle(3, 8).map[(Vector, Vector)](circleVector => (circleVector, IndexedTransformation.fromCircleToIndex(circleVector))).filter(_._1.head >= 0).toSeq,
        Color.rgb(0, 0, 0)
      )),
      onDemand("RST with lines – 2-cube", title => scatter3dLinesDiv(
        title,
        cubeWithLineRST.filter(_._1.head >= 0),
        Color.rgb(0, 0, 0)
      )),
      onDemand("RST with lines – 2-sphere", title => scatter3dLinesDiv(
        title,
        cubeWithLineRST.map(line => (IndexedTransformation.fromIndexToCircle(line._1).get, IndexedTransformation.fromIndexToCircle(line._2).get)).filter(_._1.head >= 0),
        Color.rgb(0, 0, 0)
      )),
      onDemand("RST evaluation by neighbourhood", title => {

        val dimension = 4
        val radius = 8

        val plotDiv = div()

        val evaluation = Evaluation.evaluationByNeighbourhood(dimension, radius).toSeq
          .map(_.map(_.toDegrees))
          .transpose

        val dataRef = histogram
          .x(evaluation(0).toJSArray)
          .name("No transform")
          .xbins(Bin.start(0).end(360).size(1))

        val dataTest = histogram
          .x(evaluation(1).toJSArray)
          .name("Restricted space transformation")
          .xbins(Bin.start(0).end(360).size(1))

        val layout = Layout
          .title(s"Angles between neighbours – dimension $dimension, radius = $radius")
          .showlegend(true)

        Plotly.newPlot(plotDiv.ref, js.Array(dataRef, dataTest), layout = layout)

        plotDiv
      }),
      onDemand("RST evaluation by difference", title => {

        val dimension = 6
        val radius = 3

        val plotDiv = div()

        val data = histogram
          .x(evaluationDiff(dimension, radius).map(_.toDegrees).toJSArray)
          .xbins(Bin.start(0).end(360).size(0.1))

        val layout = Layout
          .title(s"Angles diff to max magnitude – dimension $dimension, radius = $radius")
          .showlegend(true)

        Plotly.newPlot(plotDiv.ref, js.Array(data), layout = layout)

        plotDiv
      }),
      onDemand("RST evaluation by a single point", title => {

        val evaluation = singleEvaluation(30)

        val plotDiv = div()

        val referenceData = linechart
          .name("Reference")
          .setMode(lines)
          .y(evaluation(0).toJSArray)
        val testData = linechart
          .name("Restricted space transformation")
          .setMode(lines)
          .y(evaluation(1).toJSArray)

        val layout = Layout
          .title("Evaluation")
          .showlegend(true)
          .xaxis(axis.title("dimension"))
          .yaxis(axis.title("angle"))

        Plotly.newPlot(plotDiv.ref, js.Array(referenceData, testData), layout)

        plotDiv
      }),
      reloadOnDemand("ParetoFront generation 2d", title => scatter3dDiv(
        title,
        new ParetoFront(2, 128).front.map(_ :+ 0),
        Seq(0.0 at 3)
      )),
      reloadOnDemand("ParetoFront generation 3d", title => scatter3dDiv(
        title,
        new ParetoFront(3, 128).front,//.map(_ :+ 0),
        Seq(0.0 at 3)
      )),
    )
  }

  val elementDemo: Demo = new Demo {
    def title: String = "Regular directions"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

  def mainTest(args: Array[String]): Unit = {
    println(RegularDirectionsDemo.elementDemo)
  }

}
