package plotlyjs.demo.demo

import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.PlotMode.{lines, markers}
import org.openmole.plotlyjs.PlotlyImplicits._
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import plotlyjs.demo.directions.restrictedspacetransformation.v4.IndexVectors._
import plotlyjs.demo.utils.PointSet._
import plotlyjs.demo.utils.Vectors._
import plotlyjs.demo.utils.{Colors, Data, PointSet, Utils, Vectors}
import plotlyjs.demo.utils.Colors.{ImplicitColor, implicitToOMColor}

import scala.:+
import scala.math.{Pi, atan2, cos, random, sin}
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

    def cartesianToPolar(vector: Vector): Vector = {
      val r = norm(vector)
      val x = vector(0)
      val y = vector(1)
      val theta = atan2(y, x).toDegrees
      Seq(r, theta)
    }

    def polarToCartesian(vector: Vector): Vector = {
      val r = vector(0)
      val theta = vector(1).toRadians
      val x = r * cos(theta)
      val y = r * sin(theta)
      Seq(x, y)
    }

    def cartesianPlaneComponent(vector: Vector, i: Int): Vector = {
      vector(i) * polarToCartesian(Seq(1, 360 * i/vector.dimension))
    }

    def toCartesianPlane(vector: Vector): Vector = {
      (0 until vector.dimension).map(i => cartesianPlaneComponent(vector, i)).reduce(_ + _)
    }

    def starDataSeq(radius: Double)(index: Int, vector: Vector): Seq[PlotData] = {
      val dimension = vector.dimension
      val center = toCartesianPlane(vector)
      (0 until dimension).map(i => {
        val rayEnd = center + radius * cartesianPlaneComponent(vector, i)
        val cartesianCoordinates = Seq(center, rayEnd).transpose
        val polarCoordinates = Seq(center, rayEnd).map(cartesianToPolar).transpose//[Double]
        scatter/*Polar*/
          //.name("star")
          /*
          .r(polarCoordinates(0).toJSArray)
          .theta(polarCoordinates(1).toJSArray)
          */
          .x(cartesianCoordinates(0).toJSArray)
          .y(cartesianCoordinates(1).toJSArray)
          .setMode(lines)
          .line(line
            .width(2)
            .set(Seq(i.toDouble/dimension, 1, 0.5).fromHSLtoRGB)
          )
          //.setMode(markers)
          .marker(marker.size(8))
          .hoverinfo("none")
          ._result
      }) :+ scatter
        .x(Seq(center(0)).toJSArray)
        .y(Seq(center(1)).toJSArray)
        .customdata(Seq(index.toString).toJSArray)
        ._result
    }

    val plotDiv = div()

    val dimension = 5

    val radianObjectiveThetas = (0 until dimension).map(_.toDouble/dimension * 2*Pi)
    val degreeObjectiveThetas = radianObjectiveThetas.map(_.toDegrees)
    val colors = degreeObjectiveThetas.map(theta => Seq(theta/360, 1, 0.5).fromHSLtoRGB.withAlpha(0.8))
    val cartesianObjectives = radianObjectiveThetas.map(theta => Seq(Math.cos(theta), Math.sin(theta)))
    val objectivesDataSeq = degreeObjectiveThetas.zipWithIndex.map { case (theta, index) =>
      scatterPolar
        .name(s"Objective ${index + 1}")
        .r(js.Array(1, 0))
        .theta(js.Array(theta, 0))
        .setMode(lines)
        .line(line
          .set(colors(index))
        )
        ._result
    }
    def fromNormalizedSpaceToPolar(vector: Vector) = {
      def fromNormalizedSpaceToCartesian(vector: Vector) = vector
        .zip(cartesianObjectives)
        .map { case (c, o) => c * o }
        .reduce(_ + _)
      cartesianToPolar(fromNormalizedSpaceToCartesian(vector))
    }

    val p = 3
    val cubeCorner = Data.lowCorner(dimension, p)
    val sphereCorner = Data.lowSphericalCorner(dimension, p)
    val roundSimplex = Data.lowSphericalCorner(dimension, p).map(normalize)
    val doubleRoundSimplex = roundSimplex ++ roundSimplex.map(1 - _)
    val testShape = cubeCorner
    val results = Data.dim8Sample100.map(_.take(dimension))

    val pointSet = new PointSet(testShape)// ++ results)
      .optimizationProblems(Seq.fill(testShape.head.size)(MIN))
      .lowerPlotIsBetter
    val projectedOutputs = pointSet.spaceNormalizedOutputs//.map(orthogonalComponent(1 at dimension))
    lazy val allStars = projectedOutputs.zipWithIndex.flatMap { case (v, i) => starDataSeq(0.05)(i, v) }

    case class Barycenter(r: Double, theta: Double, pointSetIndex: Int)
    val barycenters = projectedOutputs
      /*.map(fromNormalizedSpaceToPolar)*/
      .map(toCartesianPlane)
      .map(cartesianToPolar)
      .zipWithIndex.map { case (p, pointSetIndex) => Barycenter(p(0), p(1), pointSetIndex) }

    def scatterPolarData(name: String, rawOutputs: Seq[Seq[Double]], barycenters: Seq[Barycenter], color: Color): PlotData = {
      scatterPolar
        .name(name)
        .r(barycenters.map(_.r).toJSArray)
        .theta(barycenters.map(_.theta).toJSArray)
        .setMode(markers)
        .set(marker
          .size(4)
          .symbol(circle)
          .color(color)
          .opacity(1/*0.5*/))
        .fillPolar(ScatterPolar.none)
        .hoverinfo("none")//.hoverinfo("text") //TODO temporaly disabled
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
      .xaxis(axis.visible(false))
      .yaxis(axis.visible(false))
      /*
      .polar(polar
        .bgcolor(
          Color.rgb(255, 255, 255)
          //Color.rgb(245, 245, 245)
          //Color.rgb(128, 128, 128)
          //Color.rgb(0, 0, 0)
        )
        .angularAxis(axis
          .visible(false)
          /*
          .showticklabels(false)
          .linewidth(2)
          ///.gridcolor(Color.rgba(12, 12, 12, 0.2))
          .ticks(TickType.outside)
          */
        )
        .radialAxis(axis
          .visible(false)
          /*
          .showticklabels(false)
          .linewidth(0)
          .ticks(TickType.none)
          */
        )

      )
      */

    val allData = /*geometryDataSeq ++ resultsDataSeq ++ objectivesDataSeq ++ */allStars
    Plotly.newPlot(plotDiv.ref, allData.toJSArray, layout)

    def get[A](plotData: PlotData, key: String, index: Int): Option[A] = entries(plotData).filter(_._1 == key).headOption.map(_._2.asInstanceOf[scala.scalajs.js.Array[A]](index))
    var tracesDisplayedCount  = 0
    plotDiv.ref.on("plotly_hover", pointsData => {
      val pointData = pointsData.points.head
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
        //val plotOutput = projectedOutputs(pointSetIndex)

        val plotDataSeq = (objectivesDataSeq.zipWithIndex map { case (dataObjective, index) =>

          val objectiveROption = get[Double](dataObjective, "r", 0)
          val objectiveThetaOption = get[Double](dataObjective, "theta", 0)
          if (objectiveROption.isDefined && objectiveThetaOption.isDefined) {
            val objectiveR = objectiveROption.get
            val objectiveTheta = objectiveThetaOption.get
            val cartesianObjective = polarToCartesian(Seq(objectiveR, objectiveTheta))

            val component = plotOutput(index)
            val componentVector = component * cartesianObjective
            val polarComponentVector = cartesianToPolar(componentVector)

            scatterPolar
              .r(js.Array(0, polarComponentVector(0)))
              .theta(js.Array(0, polarComponentVector(1)))
              .setMode(lines)
              .line(line
                .width(8)
                .set(colors(index))
              )
              .hoverinfo("none")
              ._result
          } else {
            scatterPolar._result
          }
        }) :+ scatterPolar
          .r(js.Array(r))
          .theta(js.Array(t))
          .set(marker
            .size(8)
            .symbol(circle.open)
            .color((1.0 at 3) * 0.5)
          )
          .hoverinfo("none")
          ._result

        Plotly.deleteTraces(plotDiv.ref, (0 until tracesDisplayedCount).map(_ + allData.size).map(_.toDouble).toJSArray)
        Plotly.addTraces(plotDiv.ref, plotDataSeq.map(Option(_).orUndefined).toJSArray)
        tracesDisplayedCount = plotDataSeq.size
      }
    })

    plotDiv
  }

  val elementDemo: ElementDemo = new ElementDemo {
    def title: String = "Pareto bis"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
