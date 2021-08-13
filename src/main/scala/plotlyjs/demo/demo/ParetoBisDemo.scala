package plotlyjs.demo.demo

import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.HoverMode.closest
import org.openmole.plotlyjs.PlotMode.{lines, markers, markersAndText}
import org.openmole.plotlyjs.PlotlyImplicits._
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import plotlyjs.demo.utils.Colors.{ImplicitColor, implicitToOMColor}
import plotlyjs.demo.utils.PointSet._
import plotlyjs.demo.utils.Utils.SkipOnBusy
import plotlyjs.demo.utils.vector.Vectors._
import plotlyjs.demo.utils.{Basis, ParetoFront, PointSet, Utils}

import scala.math.Numeric.BigDecimalAsIfIntegral.abs
import scala.math._
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.Object.entries

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

object ParetoBisDemo {

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

    def paretoFrontDiv(paretoFrontPoints: Seq[Vector]) = {
      val dimension = paretoFrontPoints.head.dimension
      val downHalfDimension = dimension / 2
      val upHalfDimension = ceil(dimension / 2.0).toInt

      val basis = new StarBasis(dimension)

      val spaceNormalObjectives = (0 until dimension).map((0 at dimension).replace(_, 1))
      val cartesianObjectives = spaceNormalObjectives.map(basis.transform)
      val polarObjectives = cartesianObjectives.map(polarFromCartesian)
      val colors = polarObjectives.map(vector => Seq(((vector(1) + 360) % 360) / 360, 1, 0.5).fromHSLtoRGB.opacity(0.5))
      val objectivesDataSeq = (0 until dimension).flatMap(i => { //TODO add ticks and values on axes
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

      val pointSet = new PointSet(paretoFrontPoints)
        .optimizationProblems(MIN at dimension) //To configure with metadata.
        .lowerPlotIsBetter //Reverses MAX dimensions.

      case class IndexedPoint(x: Double, y: Double, index: Int)
      val points = pointSet
        .spaceNormalizedOutputs
        .map(basis.transform)
        .zipWithIndex.map { case (point, index) => IndexedPoint(point(0), point(1), index) }

      val markerSize = 8
      val paretoFrontData = scatter
        .x(points.map(_.x).toJSArray)
        .y(points.map(_.y).toJSArray)
        .setMode(markers)
        .set(marker
          .size(markerSize)
          .symbol(circle)
          .opacity(0.5))
        .hoverinfo("none")
        .customdata(points.map(_.index.toString).toJSArray)
        ._result

      val borderShape = {

        val radius = {
          //basis.transform((1 at upHalfDimension) ++ (0 at dimension - upHalfDimension)).norm //Leave space for the componentSum to display...
          points.map(p => Seq(p.x, p.y).norm).max //... or fit the points.
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

      //Display
      val plotDiv = div()
      val dataSeq = objectivesDataSeq :+ paretoFrontData
      val size = 800
      Plotly.newPlot(
        plotDiv.ref,
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
          .showlegend(false)
          .shapes(if(dimension == 2) js.Array() else js.Array(borderShape))
          .hovermode(closest)
      )
      //

      //Events
      def get[A](plotData: PlotData, key: String, index: Int): Option[A] = entries(plotData).filter(_._1 == key).headOption.map(_._2.asInstanceOf[scala.scalajs.js.Array[A]](index))

      var tracesDisplayedCount = 0

      def addTraces(plotDataSeq: Seq[PlotData]): Unit = {
        Plotly.addTraces(plotDiv.ref, plotDataSeq.map(Option(_).orUndefined).toJSArray)
        tracesDisplayedCount += plotDataSeq.size
      }

      def deleteTraces(): Unit = {
        Plotly.deleteTraces(plotDiv.ref, (0 until tracesDisplayedCount).map(_ + dataSeq.size).map(_.toDouble).toJSArray)
        tracesDisplayedCount = 0
      }

      val rawOutputCoordinates = Var(div(""))

      def eventHandler(pointsData: PointsData): Unit = {
        println("eventHandler called")
        val pointData = pointsData.points.head

        get[String](pointData.data, "customdata", pointData.pointNumber).map(_.toInt).foreach(index => {
          val indexedPolarPoint = points(index)
          val plotOutput = pointSet.spaceNormalizedOutputs(index)

          var cartesianEnd = 0.0 at 2

          lazy val focusedPoint = {
            Seq(
              scatter
                .x(js.Array(indexedPolarPoint.x))
                .y(js.Array(indexedPolarPoint.y))
                .set(marker
                  .size(markerSize + 4)
                  .symbol(circle.open)
                  .color(0.5 at 3)
                )
            )
          }
          lazy val coordinateStar = {
            (0 until dimension).map(i => {
              val cartesianComponentVector = basis.component(plotOutput, i)
              val starPolarCoordinates = Seq(0.0 at 2, cartesianComponentVector).transpose
              scatter
                .x(starPolarCoordinates(0).toJSArray)
                .y(starPolarCoordinates(1).toJSArray)
                .setMode(lines)
                .line(line
                  .width(8)
                  .color(colors(i))
                )
              /*
                .marker(marker
                  .size(0)
                )
                */
            })
          }
          lazy val componentSum = {
            plotOutput.zipWithIndex.sortBy({ case (c, _) => abs(c)}).reverse.map(_._2).map(i => {
              val cartesianComponentVector = basis.component(plotOutput, i)
              val cartesianBegin = cartesianEnd
              cartesianEnd = cartesianBegin + cartesianComponentVector
              val sumPolarCoordinates = Seq(cartesianBegin, cartesianEnd).transpose
              scatter
                .x(sumPolarCoordinates(0).toJSArray)
                .y(sumPolarCoordinates(1).toJSArray)
                .setMode(lines)
                .line(line
                  //.width(1)
                  .dash("dot")
                  .color(/*(0.5 at 3)*/colors(i).opacity(0.5))
                )
            })
          }
          lazy val oneObjectiveCompromise = { //very few compromises in high dimension
            val neighbourhood = ParetoFront.oneObjectiveCompromiseGraph(pointSet.spaceNormalizedOutputs, plotOutput)
            neighbourhood.arrows.flatMap(arrow => {
              val coordinates = Seq(arrow.tail, arrow.head).map(basis.transform).transpose
              Seq(
                scatter
                  .x(coordinates(0).toJSArray)
                  .y(coordinates(1).toJSArray)
                  .setMode(lines)
                  .line(line
                    .width(1)
                    .color(colors(arrow.weight).opacity(1.0))
                  ),
              )
            })
          }
          lazy val multiObjectiveCompromise = {
            val neighbourhood = ParetoFront.compromise(pointSet.spaceNormalizedOutputs, plotOutput)
            neighbourhood.flatMap(vertexAndWeight => {
              val coordinates = Seq(vertexAndWeight.vertex).map(basis.transform).transpose
              Seq(
                scatter
                  .x(coordinates(0).toJSArray)
                  .y(coordinates(1).toJSArray)
                  .marker(marker
                    .size(markerSize + vertexAndWeight.weight)
                    .symbol(circle.open)
                    .color(0.5 at 3)
                  )
              )
            })
          }
          val plotDataBuilderSeq = coordinateStar ++ componentSum ++ multiObjectiveCompromise ++ oneObjectiveCompromise
          val plotDataSeq = plotDataBuilderSeq.map(_
            .hoverinfo("none")
            ._result
          ).reverse

          deleteTraces()
          addTraces(plotDataSeq)

          val textDiv = div()
          textDiv.ref.innerHTML = "Model output :<br>" + (pointSet.rawOutputs(index).zipWithIndex map { case (c, i) => s"o${i + 1} : $c" }).mkString("<br>")
          rawOutputCoordinates.set(textDiv)
        })
      }

      val skipOnBusy = new SkipOnBusy
      plotDiv.ref.on("plotly_hover", pointsData => skipOnBusy.skipOnBusy(eventHandler(pointsData)))
      plotDiv.ref.on("plotly_doubleclick", _ => skipOnBusy.skipOnBusy(deleteTraces()))
      //

      div(
        plotDiv,
        div(child <-- rawOutputCoordinates.signal)
      )
    }

    div(
      paretoFrontDiv(Utils.randomizeDimensions(new ParetoFront(2, 42).front)),
      paretoFrontDiv(Utils.randomizeDimensions(new ParetoFront(3, 42).front)),
      paretoFrontDiv(Utils.randomizeDimensions(new ParetoFront(4, 42).front)),
      paretoFrontDiv(Utils.randomizeDimensions(new ParetoFront(5, 42).front)),
    )
  }

  val elementDemo: Demo = new Demo {
    def title: String = "Pareto bis"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
