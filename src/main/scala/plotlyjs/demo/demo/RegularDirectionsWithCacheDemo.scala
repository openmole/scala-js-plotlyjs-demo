package plotlyjs.demo.demo

import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.PlotMode.lines
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import plotlyjs.demo.directions.buildingmethod.BuildingMethodWithCache
import plotlyjs.demo.directions.buildingmethod.BuildingMethodWithCache.RecursionCall
import plotlyjs.demo.utils.Graph
import plotlyjs.demo.utils.Graph.ImplicitTail

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
object RegularDirectionsWithCacheDemo {

  lazy private val sc = sourcecode.Text {

    //val resultsSeq = RegularDirectionsWithCache.parametersLines.map(_.map(_.map(p => Seq(p(0), p(1).toDegrees))))
    //  .reverse.head
    val graph = BuildingMethodWithCache.recursionGraph(RecursionCall(10, Math.PI/4 / 4, 1))

    def lineSeqFrom(graph: Graph[RecursionCall]) = {
      graph.arrows.map(arrow => Seq(arrow._1, arrow._2).map(recursionCall => Seq(recursionCall.dim.toDouble, recursionCall.angleStep.toDegrees)))
    }

    def graphDiv(graph: Graph[RecursionCall]) = {
      val plotDiv = div()

      val resultsSeq = lineSeqFrom(graph)

      val dataSeq = resultsSeq.map(resultLine => {
        val xy = resultLine.transpose
        scatter
          .x(xy(0).toJSArray)
          .y(xy(1).toJSArray)
          //.set(marker.size(1))
          .setMode(lines)
          .line(line
            .width(1)
            .color(Color.rgb(0, 0, 0))
          )
          ._result
      })

      val layout = Layout
        .showlegend(false)
        .title("nSphereCovering parameters")
        .xaxis(axis.title("dim"))
        .yaxis(axis.title("alphaStep"))
        ._result

      Plotly.newPlot(plotDiv.ref, dataSeq.toJSArray, layout)

      plotDiv
    }

    div(
      graphDiv(graph),
      graphDiv(BuildingMethodWithCache.substituteGroups(graph, Math.PI/4 / 32)
        .map(_.map(Graph(_)).reduceLeft((g1, g2) => {
          g1 + (g1.vertices.head --> g2.vertices.head)
        })).reduce(_ ++ _)
      ),
    )
  }

  val elementDemo: Demo = new Demo {
    override def isLazy = true
    def title: String = "Regular directions with cache"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
