package plotlyjs.demo


import org.openmole.plotlyjs.PlotMode.{markers, markersAndText}
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import org.openmole.plotlyjs.PlotlyImplicits._
import org.openmole.plotlyjs.plotlyConts._
import org.scalajs.dom.raw.Element

import scala.scalajs.js.JSConverters._
import scala.scalajs.js
import com.raquo.laminar.api.L._
import scaladget.svg.path.Path

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

object ParetoDemo {

  import org.openmole.plotlyjs.ScatterPolarDataBuilder._

  val sc = sourcecode.Text {

    val plotDiv = div()

    //    val results = Seq(
    //      Seq(15.0, 3.0, 7.0),
    //      Seq(4.0, 9.0, 5.0),
    //      Seq(8.0, 12.0, 11.0),
    //      Seq(6.0, 8.0, 10.0),
    //      Seq(8.0, 2.0, 6.0),
    //      Seq(11.0, 9.0, 2.0)
    //    )

    val results = Data.dim8Sample100

    val cpaVariability = Seq(0.0686544468026073, 0.08720490767943528, 0.1004942957820982, 0.11953341441462292, 0.1340278943366293, 0.14455564337712556, 0.15612690327097392, 0.18940249433650735)
    val maxCPAVariability = cpaVariability.max

    val rng = new Random(7)

    val repetitions = (0 to 100).toJSArray.map { _ => rng.between(1, 100) }

    //        val results = Seq(
    //          Seq(1 / 34.080, 0.666),
    //          Seq(1 / 34.708, 0.669),
    //          Seq(1 / 38.329, 0.680),
    //          Seq(1 / 40.587, 0.702),
    //          Seq(1 / 37.012, 0.678),
    //          Seq(1 / 39.921, 0.699),
    //          Seq(1 / 39.682, 0.687),
    //          Seq(1 / 41.428, 0.705),
    //          Seq(1 / 41.596, 0.720),
    //          Seq(1 / 41.439, 0.717)
    //        )

    //        val results = Seq(
    //          Seq(1.0, 8.0),
    //          Seq(2.0, 6.0),
    //          Seq(3.0, 4.0),
    //          Seq(5.0, 2.0),
    //          Seq(7.0, 1.0),
    //          Seq(8.0, 0.0)
    //        )

    //        val results = Seq(
    //          Seq(1.0, 8.0,4.0,1.0),
    //          Seq(2.0, 6.0,2.0,5.0),
    //          Seq(3.0, 4.0,7.0,4.0),
    //          Seq(5.0, 2.0,3.0,5.0),
    //          Seq(7.0, 1.0,2.0,10.0),
    //          Seq(8.0, 0.0,4.0,8.0)
    //        )


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

    val objectiveNames = cpaVariability.zipWithIndex map { case (c, ind) => s"Goal $ind [${(c * 100).toInt}%]" }

    //DISPLAY OBJECTIVES
    val objectiveThetas = (0 to nbObjectives - 1).toArray.map {
      _ * TWO_PI / nbObjectives
    }

    val cartesianObjectives = objectiveThetas.map { theta =>
      (Math.cos(theta), Math.sin(theta))
    }

    //val objectiveRs = (0 to nbObjectives - 1).toArray.map { _ => 1.2 }

    val dataObjectives = objectiveThetas.zip(objectiveNames).zipWithIndex.map { case ((t, name), ind) => //case(t,name)=>
      scatterPolar.
        r(js.Array(0.4)).
        theta(js.Array(t * TO_DEGREES)).
        text(js.Array(name)).
        fillPolar(ScatterPolar.toself).
        textPosition(TextPosition.topCenter).
        setMode(markersAndText).
        set(marker.size(30).color(colors(ind)).symbol(square)
      )._result
    }.toSeq

    //GET ALL MAX / OBJECTIVE
    val maxs = results.transpose.map {
      _.max
    }

    //DISPLAY PARETO POINTS
    lazy val normalizedObjectiveResults = results.map { r =>
      normalization(r)
    }

    //PARETO NORMALIZATION (values from 0 to 1) AND ORDER INVERSION (minimization to maximization)
    def normalization(sequence: Seq[Double]) = {
      sequence.zipWithIndex.map { case (el, ind) => 1 - el / maxs(ind) }
    }

    println("RESULTS " + results)
    println("NORMALIZED " + normalizedObjectiveResults)


    // BARYCENTER COMPUTATION
    val cartesianBarycenters = (normalizedObjectiveResults).map { weights =>

      val objSum = weights.sum
      val weightedCoord = (weights zip cartesianObjectives) map { case (w, (x, y)) =>
        (w * x, w * y)
      }

      val weightedCoordSum = weightedCoord.reduceLeft[(Double, Double)] { case ((x1, y1), (x2, y2)) => (x1 + x2, y1 + y2) }

      (weightedCoordSum._1 / objSum, weightedCoordSum._2 / objSum)

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


    val barycenters = (polarBarycenters zip (repetitions zip (results.map { g => s"(${g.mkString(",")})" }))).map { case (((r, theta), (repetition, label))) =>
      val index = (thetaSectors.search(theta).insertionPoint % nbObjectives)

      Barycenter(r, theta, index, cpaVariability(index), repetition, label)
    }

   // println("baricenters " + barycenters)
   // println("Labels " + results.map { g => s"(${g.mkString(",")})" }.toJSArray)
    val barycenterDataSeq = barycenters.groupBy {
      _.sector
    }.map { case (sector, b) =>
      val op = b.map {
        _.cpaVariability / maxCPAVariability
      }.toJSArray

      scatterPolar.
        r(b.map {
          _.r
        }.toJSArray).
        theta(b.map {
          _.theta
        }.toJSArray.map {
          _ * TO_DEGREES
        }).
        text(b.map {
          _.label
        }.toJSArray).
        hovertemplate("<b>%{text}</b>").
        fillPolar(ScatterPolar.none).
        setMode(markers).set(
        marker
          .opacity(op)
          .size(b.map {
            _.nbRepetitions.toDouble / 4 + 10
          }.toJSArray).set(colors(sector)).set(line.width(2).color(Color.rgb(65, 65, 65))))._result
    }


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

  val elementDemo = new ElementDemo {
    def title: String = "Pareto"

    def code: String = sc.source

    def element: HtmlElement = sc.value
  }
}
