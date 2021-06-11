package plotlyjs.demo

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.openmole.plotlyjs.PlotMode.{markers, markersAndText}
import org.openmole.plotlyjs.PlotlyImplicits._
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import org.scalajs.dom.html
import plotlyjs.demo.PointSet._
import sourcecode.Text

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.Object.entries
import scala.util.Random

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

  private val sc = sourcecode.Text {

    val plotDiv = div()

    val lowCorner = Data.lowCorner(8, 2).map(_.map(_ * 1))

    val results = Data.dim8Sample100

    val nbObjectives = lowCorner.head.size

    val TWO_PI = 2 * Math.PI
    val TO_DEGREES = 180 / Math.PI

    //DISPLAY OBJECTIVES
    val objectiveThetas = (0 until nbObjectives).toArray.map {
      _ * TWO_PI / nbObjectives
    }

    val cartesianObjectives = objectiveThetas.map { theta =>
      (Math.cos(theta), Math.sin(theta))
    }

    //val objectiveRs = (0 to nbObjectives - 1).toArray.map { _ => 1.2 }

    val dataObjectiveSeq = objectiveThetas.map { t =>
      scatterpolar.
        r(js.Array(1)).
        theta(js.Array(t * TO_DEGREES)).
        //text(js.Array(name)).
        fillPolar(ScatterPolar.toself).
        textPosition(TextPosition.topCenter).
        set(markersAndText).
        set(marker
          .size(30)
          .color(Color.hsl((t * TO_DEGREES).toInt, 100, 50))
          .symbol(circle)
      )._result
    }.toSeq

    val pointSet = new PointSet(lowCorner ++ results)
      .optimizationProblems(Seq.fill(lowCorner.head.size)(MIN))
      .higherPlotIsBetter
      .normalizePlotOutputSpace
      .normalizePlotOutputPointsNorm1

    val cartesianBarycenters = pointSet.plotOutputs.map(
      _.zip(cartesianObjectives) map {
        case (w, (x, y)) => (w * x, w * y)
      } reduce[(Double, Double)] {
        case ((x1, y1), (x2, y2)) => (x1 + x2, y1 + y2)
      })

    val polarBarycenters = cartesianBarycenters.map { case (x, y) =>
      val squaredSum = Math.sqrt(x * x + y * y)
      val r = squaredSum
      val theta = {
        val t = 2 * Math.atan(y / (squaredSum + x))
        if (t < 0) t + TWO_PI
        else t
      }
      (r, theta)
    }

    case class Barycenter(r: Double, theta: Double, pointSetIndex: Int)

    val barycenters = polarBarycenters.zipWithIndex.map { case ((r, theta), pointSetIndex) => Barycenter(r, theta, pointSetIndex) }

    def scatterPolarData(name: String, pointSet: PointSet, barycenters: Seq[Barycenter], color: Color): PlotData = {
      scatterpolar
        .name(name)
        .r(barycenters.map(_.r).toJSArray)
        .theta(barycenters.map(_.theta * TO_DEGREES).toJSArray)
        .set(markers)
        .set(marker
          .symbol(circle)
          .color(color)
          .opacity(0.5))
        .fillPolar(ScatterPolar.none)
        .hoverinfo("none")//.hoverinfo("text") TODO temporaly disabled
        .text(pointSet.rawOutputs.map(p => s"Model output :<br>${
          (p.zipWithIndex map { case (c, i) => s"o${i+1} : $c" }).mkString("<br>")
        }").toJSArray)
        .customdata(barycenters.map(_.pointSetIndex.toString).toJSArray)
        ._result
    }

    val lowCornerData = scatterPolarData(
      "Low corner",
      pointSet.slice(0, lowCorner.size), //TODO safe to slice a PointSet ? Consider create a PointSetSlice type
      barycenters.slice(0, lowCorner.size),
      Color.rgb(0, 0, 0))
    val resultsData = scatterPolarData(
      "Results",
      pointSet.slice(lowCorner.size, pointSet.size),
      barycenters.slice(lowCorner.size, pointSet.size),
      Color.rgb(255, 0, 0))

    /*
    val barycenterDataSeq = Seq(scatterpolar.
        r(barycenters.map(_.r).toJSArray).
        theta(barycenters.map(_.theta).toJSArray.map {
          _ * TO_DEGREES
        }).
        text(barycenters.map(_.label).toJSArray).
        hovertemplate("<b>%{text}</b>").
        fillPolar(ScatterPolar.none).
        set(markers).set(
        marker
          .opacity(0.5)
          .size(barycenters.map {
            _.nbRepetitions.toDouble / 4 + 10
          }.toJSArray)/*.set(colors(sector))*/.set(line.width(2).set(Color.rgb(65, 65, 65))))._result
    )
    */

    val graphWidth = 800
    val graphHeight = 800

    val layout = Layout
      .title("Pareto overview")
      .height(graphHeight)
      .width(graphWidth)
      //.showlegend(false)
      .polar(polar
        .bgcolor(Color.rgb(245, 245, 245))
        .angularAxis(axis
          .showticklabels(false)
          .linewidth(2)
          .gridcolor(Color.rgba(12, 12, 12, 0.2))
          .ticks(TickType.outside)
        )
        .radialAxis(axis
          .showticklabels(false)
          .linewidth(0)
          .ticks(TickType.none)
        )
      )

    val allData = dataObjectiveSeq ++ Seq(lowCornerData, resultsData);
    Plotly.newPlot(plotDiv.ref, allData.toJSArray, layout)

    def get[A](plotData: PlotData, key: String, index: Int): A = entries(plotData).filter(_._1 == key).head._2.asInstanceOf[scala.scalajs.js.Array[A]](index)
    val hoverTraceIndex = allData.size
    plotDiv.ref.on("plotly_hover", pointsData => {
      println("hover")

      val pointData = pointsData.points.head
      val data = pointData.data
      val index = pointData.pointNumber

      val r = get[Double](data, "r", index)
      val t = get[Double](data, "theta", index)
      val pointSetIndex = get[String](data, "customdata", index).toInt
      val plotOutput = pointSet.plotOutputs(pointSetIndex) // create non point-normalize plot points

      val plotDataSeq = dataObjectiveSeq.zipWithIndex map { case (dataObjective, index) =>
        scatterpolar
          .r(Seq(r, get[Double](dataObjective, "r", 0)).toJSArray)
          .theta(Seq(t, get[Double](dataObjective, "theta", 0)).toJSArray)
          .line(line.width(scala.math.abs(plotOutput(index)) * 16))
          .fillPolar(ScatterPolar.none)
          .hoverinfo("none")
          ._result
      }
      Plotly.addTraces(plotDiv.ref, plotDataSeq.map(Option(_).orUndefined).toJSArray, (hoverTraceIndex until hoverTraceIndex + plotDataSeq.size).map(_.toDouble).toJSArray)
/*
      plotDataSeq.zipWithIndex foreach { case (plotData, index) =>
        Plotly.addTraces(plotDiv.ref, plotData, hoverTraceIndex + index)
      }
*/
/*
      val plotData = scatterpolar
        .r(dataObjectiveSeq.map(get[Double](_, "r", 0)).flatMap(Seq(r, _)).toJSArray)
        .theta(dataObjectiveSeq.map(get[Double](_, "theta", 0)).flatMap(Seq(t, _)).toJSArray.toJSArray)
        .line(line.width(1))
        .fillPolar(ScatterPolar.none)
        .hoverinfo("none")
        ._result
*/
      //Plotly.addTraces(plotDiv.ref, plotData, hoverTraceIndex)
    })
    plotDiv.ref.on("plotly_unhover", _ => {
      println("unhover")
      Plotly.deleteTraces(plotDiv.ref, (hoverTraceIndex until hoverTraceIndex + dataObjectiveSeq.size).map(_.toDouble).toJSArray)
    })

    plotDiv
  }

  val elementDemo: ElementDemo = new ElementDemo {
    def title: String = "Pareto bis"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
