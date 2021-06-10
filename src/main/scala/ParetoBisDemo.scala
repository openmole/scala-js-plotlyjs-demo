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

    val results = Data.nCube(8, 3).filter(_.contains(0))

    val colors = Seq(
      Color.rgb(136, 34, 85),
      Color.rgb(136, 204, 238),
      Color.rgb(17, 119, 51),
      Color.rgb(221, 204, 119),
      Color.rgb(204, 102, 119),
      Color.rgb(68, 170, 153),
      Color.rgb(170, 68, 153),
      Color.rgb(0, 114, 178)
    )

    val nbObjectives = results.headOption.headOption.map(_.length).getOrElse(2)

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

    val dataObjectives = objectiveThetas.zipWithIndex.map { case (t, ind) => //case(t,name)=>
      scatterpolar.
        r(js.Array(0.4)).
        theta(js.Array(t * TO_DEGREES)).
        //text(js.Array(name)).
        fillPolar(ScatterPolar.toself).
        textPosition(TextPosition.topCenter).
        set(markersAndText).
        set(marker.size(30).color(colors(ind)).symbol(square)
      )._result
    }.toSeq

    val pointSet = new PointSet(results)
      .optimizationProblems(Seq.fill(8)(MIN))
      .higherPlotIsBetter
      .normalizePlotOutputSpace
      .normalizePlotOutputPointsNorm1

    // BARYCENTER COMPUTATION
    val cartesianBarycenters = (pointSet.plotOutputs).map { normalizedWeights =>
      val weightedCoord = (normalizedWeights zip cartesianObjectives) map { case (w, (x, y)) =>
        (w * x, w * y)
      }
      weightedCoord.reduce[(Double, Double)] { case ((x1, y1), (x2, y2)) => (x1 + x2, y1 + y2) }
    }
    println("BARYCENTERS " + cartesianBarycenters)

    // POLAR COORDINATES COMPUTATION
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

    //println("PoLar BAry " + polarBarycenters)

    case class Barycenter(r: Double, theta: Double, sector: Int, cpaVariability: Double, nbRepetitions: Int, label: String)

    // Sort points by angular sections
    val thetaSectors = {
      //      val first = -TWO_PI / (2 * nbObjectives)
      //      val last = TWO_PI / nbObjectives
      //      (BigDecimal(first) to BigDecimal(TWO_PI * (nbObjectives - 1) / nbObjectives) by BigDecimal(last)).map {
      //        _.toDouble
      //      }
      val first = -TWO_PI / (2 * nbObjectives)
      val step = TWO_PI / nbObjectives
      (BigDecimal(first) to BigDecimal(TWO_PI) by BigDecimal(step)).map {
        _.toDouble
      }.tail

    }

    println("Theta sectors " + thetaSectors)


    val barycenters = (polarBarycenters zip ((results.map { g => s"(${g.mkString(",")})" }))).map { case ((r, theta), label) =>
      val index = (thetaSectors.search(theta).insertionPoint % nbObjectives)

      Barycenter(r, theta, index, 1, 1, label)
    }

   // println("baricenters " + barycenters)
   // println("Labels " + results.map { g => s"(${g.mkString(",")})" }.toJSArray)
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


    val graphWidth = 800
    val graphHeight = 800

    val layout = Layout
      .title("Pareto overview")
      .height(graphHeight)
      .width(graphWidth)
      .showlegend(false)
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

    Plotly.newPlot(plotDiv.ref, (dataObjectives ++ barycenterDataSeq).toJSArray, layout)

    plotDiv

  }

  val elementDemo: ElementDemo = new ElementDemo {
    def title: String = "Pareto bis"

    def code: String = sc.source

    def element: HtmlElement = sc.value
  }
}
