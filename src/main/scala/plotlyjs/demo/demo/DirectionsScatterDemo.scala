package plotlyjs.demo.demo

import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.PlotlyImplicits._
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import plotlyjs.demo.directions.restrictedspacetransformation.v4.IndexedTransformation
import plotlyjs.demo.utils.vector.IntVectors._
import plotlyjs.demo.utils.PointSet.MIN
import plotlyjs.demo.utils.vector.Vectors._
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

    val dim = 5
    val results = Data.highSphericalCorner(dim, 4)//Data.simplexRST(dim, 16).toSeq//Data.lowInverseCorner(dim, 16, 2)//Data.lowSphericalCorner(dim, 16)//Data.dim8Sample100.map(_.take(dim))
    val pointSet = new PointSet(results)
      .optimizationProblems(Seq.fill(dim)(MIN))
      .lowerPlotIsBetter

    //val referenceDirections = Data.highSphericalCorner(dim, 4)
    val groupedResults = pointSet.spaceNormalizedOutputs.groupBy(v =>
      //RST4.fromSquareToCircle(RST4.fromCircleToSquare(normalize(vector)).map(c => Math.rint(4 * c))).getOrElse(Seq.fill(dim)(0.0))
      IndexedTransformation.fromCircleToIndex(v.toNorm(2))
      //referenceDirections.minBy(_ ^ v).normalize(1)
    )

    val plotDataSeq = groupedResults.map { case (indexVector, vectors) =>
      val direction = IndexedTransformation.fromIndexToCircle(indexVector).getOrElse[Vector](indexVector)
      val points = vectors.map(v => Seq(v.parallelComponent(direction).norm, v.orthogonalComponent(direction).norm))
      val xy = points.transpose
      scatter
        .name(direction.map(c => (c * 100).toInt.toDouble / 100).vectorToString)
        .x(xy(0).toJSArray)
        .y(xy(1).toJSArray)
        ._result
    }

    val layout = Layout
      .title("Directions scatter")
      .xaxis(axis
        .rangemode("tozero")
      )
      .yaxis(axis
        .rangemode("tozero")
        .scaleanchor("x")
      )

    Plotly.newPlot(plotDiv.ref, plotDataSeq.toJSArray, layout)

    plotDiv
  }

  val elementDemo: Demo = new Demo {
    override def isLazy: Boolean = true
    def title: String = "Directions Scatter"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
