package plotlyjs.demo.demo

import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.PlotMode.{lines, markers, markersAndText}
import org.openmole.plotlyjs.PlotlyImplicits._
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import plotlyjs.demo.directions.restrictedspacetransformation.v4.IndexedTransformation
import plotlyjs.demo.directions.restrictedspacetransformation.v4.IndexVectors._
import plotlyjs.demo.utils.PointSet._
import plotlyjs.demo.utils.Vectors._
import plotlyjs.demo.utils.{Data, PointSet, Utils}

import scala.math.{Pi, acos, atan2, cos}
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

    val plotDiv = div()

    val dimension = 5

    val radianObjectiveThetas = (0 until dimension).map(_.toDouble/dimension * 2*Pi)
    val degreeObjectiveThetas = radianObjectiveThetas.map(_.toDegrees)
    val colors = degreeObjectiveThetas.map(theta => Color.hsl(theta.toInt, 100, 50))
    val cartesianObjectives = radianObjectiveThetas.map(theta => Seq(Math.cos(theta), Math.sin(theta)))
    val objectivesDataSeq = degreeObjectiveThetas.zipWithIndex.map { case (theta, index) =>
      scatterPolar
        .name(s"Objective ${index + 1}")
        .r(js.Array(1, 0))
        .theta(js.Array(theta, 0))
        .fillPolar(ScatterPolar.toself)
        .textPosition(TextPosition.topCenter)
        .setMode(lines)
        .set(marker
          //.size(1)
          .color(colors(index))
          .symbol(circle))
        ._result
    }
    def fromNormalizedSpaceToPolar(vector: Vector) = {
      def fromNormalizedSpaceToCartesian(vector: Vector) = vector
        .zip(cartesianObjectives)
        .map { case (c, o) => c * o }
        .reduce(_ + _)
      def fromCartesianToPolar(vector: Vector) = {
        val r = norm(vector)
        val theta = atan2(vector(1), vector(0))
        Seq(r, theta)
      }
      fromCartesianToPolar(fromNormalizedSpaceToCartesian(vector))
    }

    val p = 6
    val cubeCorner = Data.lowCorner(dimension, p)
    val roundSimplex = Data.highSphericalCorner(dimension, p).map(normalize)
    val doubleRoundSimplex = roundSimplex ++ roundSimplex.map(1 - _)
    val testShape = cubeCorner
    val results = Data.dim8Sample100.map(_.take(dimension))

    val pointSet = new PointSet(testShape)// ++ results)
      .optimizationProblems(Seq.fill(testShape.head.size)(MIN))
      .lowerPlotIsBetter
    val diagonal = 1 at dimension
    def project(v: Vector) = orthogonalComponent(v, diagonal)
    val projectedOutputs = pointSet.spaceNormalizedOutputs
      .map(project)
    val displayOutputs = projectedOutputs
      .map(scale( 1 / (2 * cos((2 * Pi)/dimension / 2)) ))
      //.map(scale(norm(project((1 at 1) ++ (0 at dimension - 1))) / norm(project((0 at 1) ++ (1 at dimension - 1)))))

    case class Barycenter(r: Double, theta: Double, pointSetIndex: Int)
    val barycenters = projectedOutputs.map(fromNormalizedSpaceToPolar).zipWithIndex.map { case (p, pointSetIndex) => Barycenter(p(0), p(1), pointSetIndex) }

    def scatterPolarData(name: String, rawOutputs: Seq[Seq[Double]], barycenters: Seq[Barycenter], color: Color): PlotData = {
      scatterPolar
        .name(name)
        .r(barycenters.map(_.r).toJSArray)
        .theta(barycenters.map(_.theta.toDegrees).toJSArray)
        .setMode(markers)
        .set(marker
          .symbol(circle)
          .color(color)
          .opacity(1/*0.5*/))
        .fillPolar(ScatterPolar.none)
        .hoverinfo("none")//.hoverinfo("text") TODO temporaly disabled
        .text(rawOutputs.map(p => s"Model output :<br>${
          (p.zipWithIndex map { case (c, i) => s"o${i+1} : $c" }).mkString("<br>")
        }").toJSArray)
        .customdata(barycenters.map(_.pointSetIndex.toString).toJSArray)
        ._result
    }
    def scatterPolarDataSeq(name: String, rawOutputs: Seq[Seq[Double]], outputsForColoring: Seq[Vector], barycenters: Seq[Barycenter]) = {
      def get[A](seq: Seq[A], indices: Seq[Int]): Seq[A] = seq.zipWithIndex.filter(ai => indices.contains(ai._2)).map(_._1)
      outputsForColoring.indices
        .groupBy[IndexVector](i => outputsForColoring(i) * 1)
        .map { case (indexVector, indices) =>
          scatterPolarData(name + " – " + indexVector.indexVectorToString, get(rawOutputs, indices), get(barycenters, indices), Utils.randomColor)
        }
    }

    val geometryDataSeq = {
      val pointSetSlice = new pointSet.PointSetSlice(0, testShape.size)
      scatterPolarDataSeq(
        "Geometry",
        pointSetSlice.rawOutputs,
        pointSetSlice.slice(projectedOutputs),
        pointSetSlice.slice(barycenters)
      )
    }
    val resultsDataSeq = {
      val pointSetSlice = new pointSet.PointSetSlice(testShape.size, pointSet.size)
      scatterPolarDataSeq(
        "Results",
        pointSetSlice.rawOutputs,
        pointSetSlice.slice(projectedOutputs),
        pointSetSlice.slice(barycenters)
      )
    }

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

    val allData = geometryDataSeq ++ resultsDataSeq ++ objectivesDataSeq
    Plotly.newPlot(plotDiv.ref, allData.toJSArray, layout)

    def get[A](plotData: PlotData, key: String, index: Int): Option[A] = entries(plotData).filter(_._1 == key).headOption.map(_._2.asInstanceOf[scala.scalajs.js.Array[A]](index))
    var tracesDisplayedCount  = 0
    plotDiv.ref.on("plotly_hover", pointsData => {
      println(pointsData.points.size)
      pointsData.points.foreach(p => {
        val pointData = p //pointsData.points.head
        val data = pointData.data
        val index = pointData.pointNumber

        val rOption = get[Double](data, "r", index)
        val tOption = get[Double](data, "theta", index)
        val pointSetIndexOption = get[String](data, "customdata", index)
        if (rOption.isDefined && tOption.isDefined && pointSetIndexOption.isDefined) {
          val r = rOption.get
          val t = tOption.get
          val pointSetIndex = pointSetIndexOption.get.toInt

          val plotOutput = pointSet.spaceNormalizedOutputs(pointSetIndex)

          val plotDataSeq = objectivesDataSeq.zipWithIndex map { case (dataObjective, index) =>

            val objectiveROption = get[Double](dataObjective, "r", 0)
            val objectiveThetaOption = get[Double](dataObjective, "theta", 0)
            if (objectiveROption.isDefined && objectiveThetaOption.isDefined) {
              val objectiveR = objectiveROption.get
              val objectiveTheta = objectiveThetaOption.get

              scatterPolar
                .r(js.Array(r, objectiveR))
                .theta(js.Array(t, objectiveTheta))
                .setMode(lines)
                .line(line.width(scala.math.abs(plotOutput(index)) * 4).set(colors(index)))
                /*.set(markers)
              .set(marker.size(1).opacity(0.5))*/
                .fillPolar(ScatterPolar.none)
                .hoverinfo("none")
                ._result
            } else {
              scatterPolar._result
            }
          }
          Plotly.deleteTraces(plotDiv.ref, (0 until tracesDisplayedCount).map(_ + allData.size).map(_.toDouble).toJSArray)
          Plotly.addTraces(plotDiv.ref, plotDataSeq.map(Option(_).orUndefined).toJSArray)
          tracesDisplayedCount = plotDataSeq.size
        }
      })
    })

    plotDiv
  }

  val elementDemo: ElementDemo = new ElementDemo {
    def title: String = "Pareto bis"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
