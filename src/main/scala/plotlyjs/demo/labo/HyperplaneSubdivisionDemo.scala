package plotlyjs.demo.labo

import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.HistogramDataBuilder.HistogramDataBuilder
import org.openmole.plotlyjs.HoverMode.closest
import org.openmole.plotlyjs.PlotMode.{lines, markers, markersAndText}
import org.openmole.plotlyjs.all.{axis, circle, histogram, line, marker, scatter}
import org.openmole.plotlyjs._
import plotlyjs.demo.demo.Demo
import plotlyjs.demo.utils.PointSet.MIN
import plotlyjs.demo.utils.Utils.onDemand
import plotlyjs.demo.utils.vector.IntVectors._
import plotlyjs.demo.utils.vector.Vectors._
import plotlyjs.demo.utils.{Basis, Colors, Data, PointSet}
import plotlyjs.demo.utils.Colors._

import scala.math.{atan2, ceil, cos, sin}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.JSRichIterableOnce

object HyperplaneSubdivisionDemo {

  private lazy val sc = sourcecode.Text {

    def polarFromCartesian(vector: Vector): Vector = {
      val r = vector.norm
      val x = vector(0)
      val y = vector(1)
      val theta = atan2(y, x).toDegrees
      Seq(r, theta)
    }

    def cartesianFromPolar(vector: Vector): Vector = {
      val r = vector(0)
      val theta = vector(1).toRadians
      val x = r * cos(theta)
      val y = r * sin(theta)
      Seq(x, y)
    }

    class StarBasis(dimension: Int) extends Basis {

      override val size: Int = dimension

      override def basisVector(i: Int): Vector = {
        if (dimension == 2) {
          i match {
            case 0 => cartesianFromPolar(Seq(1, 0))
            case 1 => cartesianFromPolar(Seq(1, 90))
          }
        } else {
          cartesianFromPolar(Seq(1, 360 * i / dimension))
        }
      }

    }

    def starPlotDiv(dataPoints: Seq[Vector], subdivision: Int) = {
      val dimension = dataPoints.head.dimension
      val downHalfDimension = dimension / 2
      val upHalfDimension = ceil(dimension / 2.0).toInt

      val starBasis = new StarBasis(dimension)

      val spaceNormalObjectives = (0 until dimension).map((0 at dimension).replace(_, 1))
      val cartesianObjectives = spaceNormalObjectives.map(starBasis.transform)
      val polarObjectives = cartesianObjectives.map(polarFromCartesian)
      val colors = polarObjectives.map(vector => Seq(((vector(1) + 360) % 360) / 360, 1, 0.5).fromHSLtoRGB.opacity(0.5))
      val objectivesDataSeq = (0 until dimension).flatMap(i => {
        val objective = cartesianObjectives(i)
        val textPosition = 1.1 * objective
        Seq(
          scatter
            .x(js.Array(objective(0), 0))
            .y(js.Array(objective(1), 0))
            .setMode(lines)
            .line(line
              .color(colors(i))
            )
            .hoverinfo("none")
            ._result,
          scatter
            .x(js.Array(textPosition(0)))
            .y(js.Array(textPosition(1)))
            .setMode(markersAndText)
            .set(marker.set(0.0 at 4)) //TODO .marker(
            .text(s"o${i + 1}")
            .textPosition(TextPosition.middleCenter)
            .hoverinfo("none")
            ._result
        )
      })


      val pointSet = new PointSet(dataPoints)
        .optimizationProblems(MIN at dimension) //To configure with metadata.
        .lowerPlotIsBetter //Reverses MAX dimensions.

      def subdivisionReference(v: Vector) = {

        def projection(v: Vector) = {
          v.orthogonalComponent(1 at dimension)
        }

        def rint(v: Vector) = {
          v.map(scala.math.rint)
        }

        rint(dimension * {
          positiveNCube(dimension, 2)
            .map({
              val origin = rint(v)
              point => projection(origin + point)
            })
            .minBy(projection(v).distance)
        })
      }

      val grid = nGrid(dimension, subdivision).mapVertices(_.vector) //.mapVertices(subdivisionReference(_) / dimension)
      val groups = pointSet.spaceNormalizedOutputs
        .map(_ * subdivision)
        .groupBy(subdivisionReference)
      //.filter { case (reference, _) => reference.norm < subdivision * dimension * 0.5 }

      val sizes = groups.map { case (reference, group) => (reference, group.size) }

      val gridDataSeq = {
        val points = grid.vertices.toSeq.map(starBasis.transform)
        val coordinates = points.transpose
        Seq(scatter
          .x(coordinates(0).toJSArray)
          .y(coordinates(1).toJSArray)
          .set(marker //TODO .marker(
            .size(8)
            .set(circle.open) //TODO .symbol(
            .set(0.5 at 3) //TODO .color(
          )
        ) ++ grid.arrows.flatMap(arrow => {
          val points = Seq(arrow.tail, arrow.head).map(starBasis.transform)
          val coordinates = points.transpose
          Seq(
            scatter
              .line(line
                .width(1)
                .color(0.5 at 3)
              ),
            scatter
              .line(line
                .width(4)
                .color((0.5 at 3).opacity(0.1))
              ),
          ).map(_
            .x(coordinates(0).toJSArray)
            .y(coordinates(1).toJSArray)
            .setMode(lines)
          )
        })
      }.map(_
        .hoverinfo("none")
        ._result
      )

      val groupsDataSeq = groups.map { case (_, groupPoints) =>
        val groupColor = Colors.randomColor

        val points = groupPoints.map(starBasis.transform)
        val coordinates = points.transpose
        scatter
          .x(coordinates(0).toJSArray)
          .y(coordinates(1).toJSArray)
          .setMode(markers)
          .set(marker
            .size(4)
            .set(circle) //TODO .symbol(
            .set(groupColor) //TODO .color(
            .opacity(0.5)
          )
          .hoverinfo("none")
          ._result
      }

      val borderShape = {

        val radius = {
          //basis.transform((1 at upHalfDimension) ++ (0 at dimension - upHalfDimension)).norm //Leave space for the componentSum to display...
          pointSet.spaceNormalizedOutputs.map(_.norm).maxOption.getOrElse(0.0) //... or fit the points.
        }

        Shape
          .`type`("circle")
          .xref("x")
          .yref("y")
          .x0(-radius)
          .y0(-radius)
          .x1(radius)
          .y1(radius)
          .line(line
            .width(1)
            .color(0.0 at 4)
          )
      }

      val starDiv = div()
      val dataSeq = /*objectivesDataSeq ++ */ groupsDataSeq ++ gridDataSeq
      val size = 800
      Plotly.newPlot(
        starDiv.ref,
        dataSeq.toJSArray,
        Layout
          .title("Pareto")
          .height(size)
          .width(size)
          .xaxis(axis
            .visible(false)
          )
          .yaxis(axis
            .scaleanchor("x")
            .visible(false)
          )
          //.showlegend(false)
          //.shapes(if(dimension == 2) js.Array() else js.Array(borderShape))
          .hovermode(closest)
          ._result
      )

      val histogramDiv = div()
      val data = histogram
        .x(sizes.values.toJSArray)
        .xbins(Bin.start(0).end(1000).size(1))
      val layout = Layout
        .showlegend(true)
      Plotly.newPlot(histogramDiv.ref, js.Array(data), layout = layout._result)

      div(
        starDiv,
        histogramDiv,
      )
    }

    def points(dimension: Int, p: Int) = {
      val lowCorner = Data.lowCorner(dimension, p)
      lowCorner.map(_ + 0)
    }

    div(
      onDemand("3rd dimension", _ => starPlotDiv(points(3, 32), 3)),
      onDemand("4th dimension", _ => starPlotDiv(points(4, 2), 3)),
      onDemand("5th dimension", _ => starPlotDiv(points(5, 2), 3)),
      onDemand("6th dimension", _ => starPlotDiv(points(6, 2), 2)),
      onDemand("7th dimension", _ => starPlotDiv(points(7, 2), 2)),
      onDemand("8th dimension", _ => starPlotDiv(points(8, 2), 2)),
    )
  }

  val elementDemo: Demo = new Demo {
    override def isLazy: Boolean = false

    def title: String = "Hyperplane subdivision"

    def code: String = sc.source

    def element: HtmlElement = sc.value
  }

}
