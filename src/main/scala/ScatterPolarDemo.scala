package plotlyjs.demo


import org.openmole.plotlyjs.PlotMode.{markers, markersAndText}
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import org.openmole.plotlyjs.PlotlyImplicits._
import org.openmole.plotlyjs.plotlyConts._
import org.scalajs.dom.raw.Element

import scala.scalajs.js.JSConverters._
import scala.scalajs.js
import scalatags.JsDom.all._

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

object ScatterPolarDemo {

  import org.openmole.plotlyjs.ScatterPolarDataBuilder._

  val sc = sourcecode.Text {

    val plotDiv = div.render

    val results = Seq(
      Seq(15.0, 3.0, 7.0),
      Seq(4.0, 9.0, 5.0),
      Seq(8.0, 12.0, 11.0),
      Seq(6.0, 8.0, 10.0),
      Seq(8.0, 2.0, 6.0),
      Seq(11.0,9.0, 2.0)
    )

    val repetitions = Seq(100.0, 20.0, 10.0, 1.0, 25.0, 10.0, 2.0, 5.0, 5.0, 45.0).toJSArray

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
      Color.rgb(51, 34, 136)
    )

    val nbObjectives = results.headOption.headOption.map(_.length).getOrElse(2)

    val TWO_PI = 2 * Math.PI
    val TO_DEGREES = 180 / Math.PI

    val objectiveNames = Seq("Goal 1", "Goal 2", "Goal 3", "Goal 4")

    //DISPLAY OBJECTIVES
    val objectiveThetas = (0 to nbObjectives - 1).toArray.map {
      _ * TWO_PI / nbObjectives
    }

    val cartesianObjectives = objectiveThetas.map { theta =>
      (Math.cos(theta), Math.sin(theta))
    }

    //val objectiveRs = (0 to nbObjectives - 1).toArray.map { _ => 1.2 }

    val dataObjectives = objectiveThetas.zip(objectiveNames).zipWithIndex.map { case ((t, name), ind) => //case(t,name)=>
      scatterpolar.
        r(js.Array(1.1)).
        theta(js.Array(t * TO_DEGREES)).
        text(js.Array(name)).
        fillPolar(ScatterPolar.toself).
        textPosition(TextPosition.topCenter).
        set(markersAndText).set(
        marker.size(30).color(colors(ind)).symbol(square)
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

    println("PoLar BAry " + polarBarycenters)

    case class Barycenter(r: Double, theta: Double, sector: Int)

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

    val barycenters = polarBarycenters.map { case (r, theta) =>
      Barycenter(r, theta, (thetaSectors.search(theta).insertionPoint % nbObjectives))
    }

println("baricenters " + barycenters)
    val barycenterDataSeq = barycenters.groupBy{_.sector}.map { case (sector, b) =>
      println("SECTOR " + sector  + "BAR " + b)
      scatterpolar.
        r(b.map{_.r}.toJSArray).
        theta(b.map{_.theta}.toJSArray.map {
          _ * TO_DEGREES
        }).
        text(results.map { g => s"(${g.mkString(",")})" }.toJSArray).
        hovertemplate("<b>%{text}</b>").
        fillPolar(ScatterPolar.none).
        set(markers).set(marker.size(repetitions.map { r => r / 4 + 10 }).set(colors(sector)).set(line.width(2).set(Color.rgb(242, 242, 242))))._result
    }

//    val dataParetoPoints = scatterpolar.
//      r(polarBarycenters.map {
//        _._1
//      }.toJSArray).
//      theta(polarBarycenters.map {
//        _._2
//      }.toJSArray.map {
//        _ * TO_DEGREES
//      }).
//      text(results.map { g => s"(${g.mkString(",")})" }.toJSArray).
//      hovertemplate("<b>%{text}</b>").
//      fillPolar(ScatterPolar.none).
//      set(markers).set(marker.size(repetitions.map { r => r / 4 + 10 }).set(Color.rgb(30, 136, 229)).set(line.width(2).set(Color.rgb(242, 242, 242))))
//

    val layout = Layout
      .title("Pareto overview")
      .height(800)
      .width(800)
      .showlegend(false)
      .polar(polar
        .set(angularaxis
          .showticklabels(true)
          .linewidth(3)
          .ticks(TickType.outside)
        )
        .set(
          radialaxis
            .showticklabels(false)
            .linewidth(0)
            .ticks(TickType.none)
        )
      )

    Plotly.newPlot(plotDiv, (dataObjectives ++ barycenterDataSeq).toJSArray, layout)

    plotDiv.render

  }

  val elementDemo = new ElementDemo {
    def title: String = "Scatter polar"

    def code: String = sc.source

    def element: Element = sc.value
  }
}
