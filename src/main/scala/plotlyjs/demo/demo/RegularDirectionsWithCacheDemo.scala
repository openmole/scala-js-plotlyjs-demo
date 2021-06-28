package plotlyjs.demo.demo

import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.PlotMode.lines
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import plotlyjs.demo.directions.{RegularDirections, RegularDirectionsWithCache}
import plotlyjs.demo.utils.PointSet.MIN
import plotlyjs.demo.utils.Vectors._
import plotlyjs.demo.utils.{Data, PointSet}

import scala.math.abs
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

  private val sc = sourcecode.Text {

    val plotDiv = div()

    //val resultsSeq = RegularDirectionsWithCache.parametersLines.map(_.map(_.map(p => Seq(p(0), p(1).toDegrees))))
    //  .reverse.head
    val resultsSeq = RegularDirectionsWithCache.nSphereCoveringRecursionGraph(10, Math.PI/4 / 4)._2.arrows.map(arrow => Seq(arrow._1, arrow._2).map(recursionCall => Seq(recursionCall.dim.toDouble, recursionCall.angleStep.toDegrees)))

    val dataSeq = resultsSeq.map(resultLine => {
      val xy = resultLine.transpose
      scatter
        .x(xy(0).toJSArray)
        .y(xy(1).toJSArray)
        .setMode(lines)
        .line(line
          .width(1)
          .set(Color.rgb(0, 0, 0))
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


  val elementDemo: ElementDemo = new ElementDemo {
    def title: String = "Regular directions with cache"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}