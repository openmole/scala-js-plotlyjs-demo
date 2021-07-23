package plotlyjs.demo.demo

import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.PlotMode.{lines, markers, markersAndText}
import org.openmole.plotlyjs.PlotlyImplicits._
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import plotlyjs.demo.utils.Colors.{ImplicitColor, implicitToOMColor}
import plotlyjs.demo.utils.PointSet._
import plotlyjs.demo.utils.Vectors._
import plotlyjs.demo.utils.{Data, ParetoFront, PointSet}

import scala.math.Numeric.BigDecimalAsIfIntegral.abs
import scala.math.{atan2, ceil, cos, random, sin}
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

  import org.openmole.plotlyjs.ScatterPolarDataBuilder._

  lazy private val sc = sourcecode.Text {

    def polarFromCartesian(vector: Vector): Vector = {
      val r = norm(vector)
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

    def basisVector(dimension: Int, i: Int): Vector = {
      cartesianFromPolar(Seq(1, 360 * i/dimension))
    }

    def cartesianPlaneComponent(vector: Vector, i: Int): Vector = {
      vector(i) * basisVector(vector.dimension, i)
    }

    def toCartesianPlane(vector: Vector): Vector = {
      (0 until vector.dimension).map(i => cartesianPlaneComponent(vector, i)).reduce(_ + _)
    }

    val dimension = 5
    val halfDimension = ceil(dimension/2.0).toInt
    val paretoFrontPoints = (new ParetoFront(dimension, 42).front :+ ((1 at halfDimension) ++ (0 at dimension - halfDimension)))
      .map(mul((() => ceil(10 * random)) at dimension))
    //val dimension = paretoFrontPoints.head.dimension

    val spaceNormalObjectives = (0 until dimension).map((0 at dimension).replace(_, 1))
    val cartesianObjectives = spaceNormalObjectives.map(toCartesianPlane)
    val polarObjectives = cartesianObjectives.map(polarFromCartesian)
    val colors = polarObjectives.map(vector => Seq(((vector(1) + 360)%360)/360, 1, 0.5).fromHSLtoRGB.withAlpha(0.5))
    val objectivesDataSeq = (0 until dimension).flatMap(i => {
      val polar = polarObjectives(i)
      val textPosition = polarFromCartesian(1.1 * cartesianObjectives(i))
      Seq(
        scatterPolar
          .r(js.Array(polar(0), 0))
          .theta(js.Array(polar(1), 0))
          .setMode(lines)
          .line(line
            .color(colors(i))
          )
          ._result,
        scatterPolar
          .r(js.Array(textPosition(0)))
          .theta(js.Array(textPosition(1)))
          .setMode(markersAndText)
          .marker(marker.set(0.0 at 4))
          .text(s"o${i+1}")
          .textPosition(TextPosition.middleCenter)
          .hoverinfo("none")
          ._result
      )
    })

    val pointSet = new PointSet(paretoFrontPoints)
      .optimizationProblems(MIN at dimension) //To configure with metadata.
      .lowerPlotIsBetter //Reverses MAX dimensions.

    case class IndexedPolarPoint(r: Double, theta: Double, index: Int)
    val points = pointSet
      .spaceNormalizedOutputs
      .map(toCartesianPlane)
      .map(polarFromCartesian)
      .zipWithIndex.map { case (point, index) => IndexedPolarPoint(point(0), point(1), index) }

    val markerSize = 8
    val paretoFrontData = scatterPolar
      .r(points.map(_.r).toJSArray)
      .theta(points.map(_.theta).toJSArray)
      .setMode(markers)
      .set(marker
        .size(markerSize)
        .symbol(circle)
        .opacity(0.5))
      .fillPolar(ScatterPolar.none)
      .hoverinfo("none")
      .customdata(points.map(_.index.toString).toJSArray)
      ._result

    val leaveSpaceData = scatterPolar //Leaving space for graphical vector components sum.
      // No enough with many dimensions, TODO compute a theoretical value or compute the space needed from the given data
      .r(js.Array(2))
      .theta(js.Array(0))
      .marker(marker
        .set(0.0 at 4)
      )
      ._result

    //Display
    val plotDiv = div()
    val dataSeq = objectivesDataSeq :+ paretoFrontData :+ leaveSpaceData
    val size = 800
    Plotly.newPlot(
      plotDiv.ref,
      dataSeq.toJSArray,
      Layout
        .title("Pareto")
        .height(size)
        .width(size)
        .showlegend(false)
        .polar(polar
          .bgcolor(1.0 at 3)
          .angularAxis(axis.visible(false))
          .radialAxis(axis.visible(false))
        )
    )
    //

    //Events
    def get[A](plotData: PlotData, key: String, index: Int): Option[A] = entries(plotData).filter(_._1 == key).headOption.map(_._2.asInstanceOf[scala.scalajs.js.Array[A]](index))

    var tracesDisplayedCount  = 0
    val rawOutputCoordinates = Var(div(""))
    plotDiv.ref.on("plotly_hover", pointsData => {
      val pointData = pointsData.points.head

      get[String](pointData.data, "customdata", pointData.pointNumber).map(_.toInt).foreach(index => {
        val indexedPolarPoint = points(index)
        val plotOutput = pointSet.spaceNormalizedOutputs(index)

        var cartesianEnd = 0.0 at dimension

        val plotDataSeq = (0 until dimension).map(i => {
          val cartesianComponentVector = cartesianPlaneComponent(plotOutput, i)
          val starPolarCoordinates = Seq(0.0 at dimension, cartesianComponentVector).map(polarFromCartesian).transpose
          scatterPolar
            .r(starPolarCoordinates(0).toJSArray)
            .theta(starPolarCoordinates(1).toJSArray)
            .setMode(lines)
            .line(line
              .width(8)
              .color(colors(i))
            )
            .hoverinfo("none")
            ._result
        }) ++ plotOutput.zipWithIndex.sortBy({ case (c, _) => abs(c)}).reverse.map(_._2).map(i => {
          val cartesianComponentVector = cartesianPlaneComponent(plotOutput, i)
          val cartesianBegin = cartesianEnd
          cartesianEnd = cartesianBegin + cartesianComponentVector
          val sumPolarCoordinates = Seq(cartesianBegin, cartesianEnd).map(polarFromCartesian).transpose
          scatterPolar
            .r(sumPolarCoordinates(0).toJSArray)
            .theta(sumPolarCoordinates(1).toJSArray)
            .setMode(lines)
            .line(line
              //.width(1)
              .dash("dot")
              .color(/*(0.5 at 3)*/colors(i).withAlpha(0.5))
            )
            .hoverinfo("none")
            ._result
        }) :+ scatterPolar
          .r(js.Array(indexedPolarPoint.r))
          .theta(js.Array(indexedPolarPoint.theta))
          .set(marker
            .size(markerSize + 4)
            .symbol(circle.open)
            .color(0.5 at 3)
          )
          .hoverinfo("none")
          ._result

        Plotly.deleteTraces(plotDiv.ref, (0 until tracesDisplayedCount).map(_ + dataSeq.size).map(_.toDouble).toJSArray)
        Plotly.addTraces(plotDiv.ref, plotDataSeq.map(Option(_).orUndefined).toJSArray)
        tracesDisplayedCount = plotDataSeq.size

        val textDiv = div()
        textDiv.ref.innerHTML = "Model output :<br>" + (pointSet.rawOutputs(index).zipWithIndex map { case (c, i) => s"o${i+1} : $c" }).mkString("<br>")
        rawOutputCoordinates.set(textDiv)
      })
    })
    //

    div(
      plotDiv,
      div(child <-- rawOutputCoordinates.signal)
    )
  }

  val elementDemo: Demo = new Demo {
    def title: String = "Pareto bis"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
