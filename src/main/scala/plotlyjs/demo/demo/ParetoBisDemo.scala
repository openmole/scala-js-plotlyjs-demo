package plotlyjs.demo.demo

import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.PlotMode.{markers, markersAndText}
import org.openmole.plotlyjs.PlotlyImplicits._
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import plotlyjs.demo.directions.restrictedspacetransformation.v4.IndexedTransformation
import plotlyjs.demo.utils.PointSet._
import plotlyjs.demo.utils.Vectors._
import plotlyjs.demo.utils.{Data, PointSet, Utils}

import scala.math.{pow, random, sqrt}
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

    val dimension = 3

    val nbObjectives = dimension

    val TWO_PI = 2 * Math.PI
    val TO_DEGREES = 180 / Math.PI

    //DISPLAY OBJECTIVES
    val radianObjectiveThetas = (0 until nbObjectives).map(_ * TWO_PI/nbObjectives)
    val degreeObjectiveThetas = radianObjectiveThetas.map(_ * TO_DEGREES)
    val colors = degreeObjectiveThetas.map(theta => Color.hsl(theta.toInt, 100, 50))
    val cartesianObjectives = radianObjectiveThetas.map(theta => (Math.cos(theta), Math.sin(theta)))
    val dataObjectiveSeq = degreeObjectiveThetas.zipWithIndex.map { case (theta, index) =>
      scatterPolar
        .r(js.Array(1))
        .theta(js.Array(theta))
        .fillPolar(ScatterPolar.toself)
        .textPosition(TextPosition.topCenter)
        .setMode(markersAndText)
        .set(marker
          .size(30)
          .color(colors(index))
          .symbol(circle))
        ._result
    }

    val p = 16
    val shape = Data.highSphericalCorner(dimension, p).map(toNorm(1, 1))
    val testShape = shape ++ shape.map(1 - _)
    val results = Data.dim8Sample100.map(_.take(dimension))

    val pointSet = new PointSet(testShape/* ++ results*/)
      .optimizationProblems(Seq.fill(testShape.head.size)(MIN))
      .higherPlotIsBetter

    val simplexStarCenter = 0.5 at dimension
    val simplexStarNorm1 = simplexStarCenter.norm(1)
    val scaleFactor = 0.5 / (0.5 * sqrt(dimension * (dimension - 1)))
    val projectedOutputs = pointSet.spaceNormalizedOutputs.map(point => {
        if(point.norm(1) < simplexStarNorm1) {
          point.toNorm(1, simplexStarNorm1)
        } else {
          1 - (1 - point).toNorm(1, simplexStarNorm1)
        }
      })
    val positiveProjectedOutputs = projectedOutputs.map(pointOnSimplexStar => simplexStarCenter + scaleFactor * (pointOnSimplexStar - simplexStarCenter))
    assert(positiveProjectedOutputs.map(_.map(_ > 0).reduce(_ && _)).reduce(_ && _))
    val normalized1PositiveProjectedOutputs = positiveProjectedOutputs.map(normalize(1))

    val cartesianBarycenters = /*pointSet.norm1VectorNormalizedOutputs*/projectedOutputs.map(
      _.zip(cartesianObjectives) map {
        case (c, (x, y)) => (c * x, c * y)
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

    def scatterPolarData(name: String, rawOutputs: Seq[Seq[Double]], barycenters: Seq[Barycenter], color: Color): PlotData = {
      scatterPolar
        .name(name)
        .r(barycenters.map(_.r).toJSArray)
        .theta(barycenters.map(_.theta * TO_DEGREES).toJSArray)
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
    def scatterPolarDataSeq(name: String, rawOutputs: Seq[Seq[Double]], spaceNormalizedOutputs: Seq[Vector], barycenters: Seq[Barycenter]) = {
      def get[A](seq: Seq[A], indices: Seq[Int]): Seq[A] = seq.zipWithIndex.filter(ai => indices.contains(ai._2)).map(_._1)
      spaceNormalizedOutputs.indices
        .groupBy(i => IndexedTransformation.fromCircleToIndex(spaceNormalizedOutputs(i).toNorm(2)))
        .map { case (_, indices) =>
          scatterPolarData(name, get(rawOutputs, indices), get(barycenters, indices), Utils.randomColor)
        }
    }

    val geometryDataSeq = {
      val pointSetSlice = new pointSet.PointSetSlice(0, testShape.size)
      scatterPolarDataSeq(
        "Geometry",
        pointSetSlice.rawOutputs,
        projectedOutputs.slice(0, testShape.size),
        barycenters.slice(0, testShape.size)
      )
    }
    val resultsDataSeq = {
      val pointSetSlice = new pointSet.PointSetSlice(testShape.size, pointSet.size)
      scatterPolarDataSeq(
        "Results",
        pointSetSlice.rawOutputs,
        projectedOutputs.slice(testShape.size, pointSet.size),
        barycenters.slice(testShape.size, pointSet.size)
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

    val allData = dataObjectiveSeq ++ geometryDataSeq ++ resultsDataSeq
    Plotly.newPlot(plotDiv.ref, allData.toJSArray, layout)

    def get[A](plotData: PlotData, key: String, index: Int): Option[A] = entries(plotData).filter(_._1 == key).headOption.map(_._2.asInstanceOf[scala.scalajs.js.Array[A]](index))
    //val hoverTraceIndex = allData.size
    var addedTracesCount  = 0
    plotDiv.ref.on("plotly_click", pointsData => {
      //println("hover")

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

          val plotDataSeq = dataObjectiveSeq.zipWithIndex map { case (dataObjective, index) =>

            val objectiveROption = get[Double](dataObjective, "r", 0)
            val objectiveThetaOption = get[Double](dataObjective, "theta", 0)
            if (objectiveROption.isDefined && objectiveThetaOption.isDefined) {
              val objectiveR = objectiveROption.get
              val objectiveTheta = objectiveThetaOption.get

              scatterPolar
                .r(js.Array(r, objectiveR))
                .theta(js.Array(t, objectiveTheta))
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
          Plotly.addTraces(plotDiv.ref, plotDataSeq.map[js.UndefOr[PlotData]](Option(_).orUndefined).toJSArray)
          addedTracesCount += plotDataSeq.size
        }
      })
      /*
      plotDataSeq.zipWithIndex foreach { case (plotData, index) =>
        Plotly.addTraces(plotDiv.ref, plotData, hoverTraceIndex + index)
      }
*/
/*
      val plotData = scatterPolar
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
      //println("unhover")
      //Plotly.deleteTraces(plotDiv.ref, (0 until addedTracesCount).map(_ + allData.size).map(_.toDouble).toJSArray)
    })

    plotDiv
  }

  val elementDemo: ElementDemo = new ElementDemo {
    def title: String = "Pareto bis"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
