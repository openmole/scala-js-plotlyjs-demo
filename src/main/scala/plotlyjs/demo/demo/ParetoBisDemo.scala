package plotlyjs.demo.demo

import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.PlotMode.{lines, markers}
import org.openmole.plotlyjs.PlotlyImplicits._
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import plotlyjs.demo.utils.Colors.{ImplicitColor, implicitToOMColor}
import plotlyjs.demo.utils.PointSet._
import plotlyjs.demo.utils.Vectors._
import plotlyjs.demo.utils.{Data, ParetoFront, PointSet}

import scala.math.{atan2, cos, sin}
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

    val dimension = 5

    val cartesianObjectives = (0 until dimension).map((0 at dimension).replace(_, 1)).map(toCartesianPlane)
    val polarObjectives = cartesianObjectives.map(cartesianToPolar)
    val colors = polarObjectives.map(vector => Seq(((vector(1) + 360)%360)/360, 1, 0.5).fromHSLtoRGB.withAlpha(0.5))
    val objectivesDataSeq = polarObjectives.zipWithIndex.map { case (vector, index) =>
      scatterPolar
        .name(s"Objective ${index + 1}")
        .r(js.Array(vector(0), 0))
        .theta(js.Array(vector(1), 0))
        .setMode(lines)
        .line(line
          .set(colors(index))
        )
        ._result
    }

    val p = 8
    lazy val cubeCorner = Data.lowCorner(dimension, p)
    lazy val sphereCorner = Data.lowSphericalCorner(dimension, p)
    lazy val roundSimplex = Data.lowSphericalCorner(dimension, p).map(normalize)
    lazy val doubleRoundSimplex = roundSimplex ++ roundSimplex.map(1 - _)
    lazy val paretoFront = new ParetoFront(dimension, 64).front
    val testShape = paretoFront
    val results = Data.dim8Sample100.map(_.take(dimension))

    val pointSet = new PointSet(testShape)// ++ results)
      .optimizationProblems(MIN at dimension)
      .lowerPlotIsBetter
      //.higherPlotIsBetter
    val projectedOutputs = pointSet.spaceNormalizedOutputs.map(orthogonalComponent(1 at dimension))

    case class Barycenter(r: Double, theta: Double, pointSetIndex: Int)
    val barycenters = projectedOutputs.map(toCartesianPlane).map(cartesianToPolar).zipWithIndex.map { case (p, pointSetIndex) => Barycenter(p(0), p(1), pointSetIndex) }

    def scatterPolarData(name: String, rawOutputs: Seq[Seq[Double]], barycenters: Seq[Barycenter]): PlotData = {
      scatterPolar
        .name(name)
        .r(barycenters.map(_.r).toJSArray)
        .theta(barycenters.map(_.theta).toJSArray)
        .setMode(markers)
        .set(marker
          .size(4)
          .symbol(circle)
          //.color(color)
          .opacity(1/*0.5*/))
        .fillPolar(ScatterPolar.none)
        .hoverinfo("none")//.hoverinfo("text") //TODO temporaly disabled
        .text(rawOutputs.map(p => s"Model output :<br>${
          (p.zipWithIndex map { case (c, i) => s"o${i+1} : $c" }).mkString("<br>")
        }").toJSArray)
        .customdata(barycenters.map(_.pointSetIndex.toString).toJSArray)
        ._result
    }

    val geometryData = {
      val pointSetSlice = new pointSet.PointSetSlice(0, testShape.size)
      scatterPolarData(
        "Geometry",
        pointSetSlice.rawOutputs,
        pointSetSlice.slice(barycenters)
      )
    }
    val resultsData = {
      val pointSetSlice = new pointSet.PointSetSlice(testShape.size, pointSet.size)
      scatterPolarData(
        "Results",
        pointSetSlice.rawOutputs,
        pointSetSlice.slice(barycenters)
      )
    }



    //Display
    val graphWidth = 800
    val graphHeight = 800

    val plotDiv = div()
    val dataSeq = Seq(geometryData, resultsData) ++ objectivesDataSeq
    Plotly.newPlot(
      plotDiv.ref,
      dataSeq.toJSArray,
      Layout
        .title("Pareto")
        .height(graphHeight)
        .width(graphWidth)
        //.showlegend(false)
        .polar(polar
          .bgcolor(
            Color.rgb(255, 255, 255)
            //Color.rgb(245, 245, 245)
            //Color.rgb(128, 128, 128)
            //Color.rgb(0, 0, 0)
          )
          .angularAxis(axis
            .visible(false)
          )
          .radialAxis(axis
            .visible(false)
          )
        )
    )
    //



    //Events
    def get[A](plotData: PlotData, key: String, index: Int): Option[A] = entries(plotData).filter(_._1 == key).headOption.map(_._2.asInstanceOf[scala.scalajs.js.Array[A]](index))

    var tracesDisplayedCount1  = 0
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

        Plotly.deleteTraces(plotDiv.ref, (0 until tracesDisplayedCount1).map(_ + dataSeq.size).map(_.toDouble).toJSArray)
        Plotly.addTraces(plotDiv.ref, plotDataSeq.map(Option(_).orUndefined).toJSArray)
        tracesDisplayedCount1 = plotDataSeq.size
      }
    })
    //



    plotDiv
  }

  val elementDemo: ElementDemo = new ElementDemo {
    def title: String = "Pareto bis"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
