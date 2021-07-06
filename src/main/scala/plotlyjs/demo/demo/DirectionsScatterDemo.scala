package plotlyjs.demo.demo

import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.PlotlyImplicits._
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import plotlyjs.demo.utils.PointSet.MIN
import plotlyjs.demo.utils.Vectors._
import plotlyjs.demo.utils.{Data, PointSet}

import scala.scalajs.js.JSConverters.JSRichIterableOnce

/*
 * Copyright (C) 24/03/16 // mathieu.leclaire@openmole.org
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
 */
object DirectionsScatterDemo {

  private lazy val sc = sourcecode.Text {

    val plotDiv = div()

    val dim = 3
    val p = 8
    val results = Data.lowSphericalCorner(dim, p)//Data.dim8Sample100.map(_.drop(8 - dim))
    val pointSet = new PointSet(results)
      .optimizationProblems(Seq.fill(dim)(MIN))
      .lowerPlotIsBetter

    val referenceDirections = Data.highSphericalCorner(dim, 4)
    val groupedResults = pointSet.spaceNormalizedOutputs.groupBy[Vector](v =>
      //RST4.fromSquareToCircle(RST4.fromCircleToSquare(normalize(vector)).map(c => Math.rint(4 * c))).getOrElse(Seq.fill(dim)(0.0))
      referenceDirections.map(rd => (rd, v ^ rd)).minBy(_._2)._1.normalize(1)
    )

    val plotDataSeq = groupedResults.map { case (direction, vectors) =>
      val points = vectors.map(v => Seq(v.parallelComponent(direction).norm, v.orthogonalComponent(direction).norm))
      val xy = points.transpose
      scatter
        .name(direction.map(c => (c * 100).toInt.toDouble / 100).vectorToString)
        .x(xy(0).toJSArray)
        .y(xy(1).toJSArray)
        ._result
    }

    val sqrtN = scala.math.sqrt(dim)
    val layout = Layout
      .title("Directions scatter")
      .xaxis(
        axis
          .bounds(Seq(0, sqrtN).toJSArray)
          .fixedrange(true)
      )
      .yaxis(
        axis
          .bounds(Seq(0, sqrtN).toJSArray)
          .fixedrange(true)
      )

    val config = Config.autosizable(false).staticPlot(true)

    //TODO disable automatic zoom on trace focus
    Plotly.newPlot(plotDiv.ref, plotDataSeq.toJSArray, layout, config)

    plotDiv
  }


  val elementDemo: ElementDemo = new ElementDemo {
    def title: String = "Directions Scatter"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
