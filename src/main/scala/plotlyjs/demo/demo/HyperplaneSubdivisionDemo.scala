package plotlyjs.demo.demo

import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.HistogramDataBuilder.HistogramDataBuilder
import org.openmole.plotlyjs.HoverMode.closest
import org.openmole.plotlyjs.PlotMode.{lines, markers, markersAndText}
import org.openmole.plotlyjs.PlotlyImplicits._
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import plotlyjs.demo.utils.vector.IntVectors._
import plotlyjs.demo.utils.Colors.{ImplicitColor, implicitToOMColor}
import plotlyjs.demo.utils.PointSet._
import plotlyjs.demo.utils.vector.Vectors._
import plotlyjs.demo.utils._
import plotlyjs.demo.utils.vector.{IntVectors, Vectors}

import scala.math._
import scala.scalajs.js
import scala.scalajs.js.JSConverters._

/*
 * Copyright (C) 31/10/17 // mathieu.leclaire@openmole.org
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

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
        if(dimension == 2) {
          i match {
            case 0 => cartesianFromPolar(Seq(1, 0))
            case 1 => cartesianFromPolar(Seq(1, 90))
          }
        } else {
          cartesianFromPolar(Seq(1, 360 * i/dimension))
        }
      }

    }

    def starPlotDiv(dataPoints: Seq[Vector]) = {
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
            .marker(marker.set(0.0 at 4))
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
          IntVectors.positiveNCube(dimension, 2)
            .map({
              val origin = rint(v)
              point => projection(origin + point)
            })
            .minBy(projection(v).distance)
        })
      }

      val subdivision = 2
      val groups = pointSet.spaceNormalizedOutputs
        .map(_ * subdivision)
        .groupBy(subdivisionReference)
        //.filter { case (reference, _) => reference.norm < subdivision * dimension * 0.5 }

      val sizes = groups.map { case (reference, group) => (reference, group.size) }
      //sizes.foreach { case (reference, size) => println(reference.vectorToString + " " + size) }
      lazy val sizeCount = sizes.values
        .groupBy(size => size)
        .map { case (size, sizeGroup) => (size, sizeGroup.size)}
        .map[(Int, Int)](t => t)
        .toSeq
        .sortBy(_._1)

      val plotDataSeq = groups.map { case (_, groupPoints) =>
        val groupColor = Colors.randomColor

        val points = groupPoints.map(starBasis.transform)
        val coordinates = points.transpose
        scatter
          .x(coordinates(0).toJSArray)
          .y(coordinates(1).toJSArray)
          .setMode(markers)
          .set(marker
            .size(4)
            .symbol(circle)
            .color(groupColor)
            .opacity(0.5))
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
      val dataSeq = objectivesDataSeq ++ plotDataSeq
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
          .shapes(if(dimension == 2) js.Array() else js.Array(borderShape))
          .hovermode(closest)
      )

      val histogramDiv = div()
      val data = histogram
        .x(sizes.values.toJSArray)
        .xbins(Bin.start(0).end(1000).size(1))
      val layout = Layout
        .showlegend(true)
      Plotly.newPlot(histogramDiv.ref, js.Array(data), layout = layout)

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
      starPlotDiv(points(3, 64)),
      //starPlotDiv(points(4, 12)),
      //starPlotDiv(points(5, 12)),
    )
  }

  val elementDemo: Demo = new Demo {
    override def isLazy: Boolean = true
    def title: String = "Hyperplane subdivision"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
