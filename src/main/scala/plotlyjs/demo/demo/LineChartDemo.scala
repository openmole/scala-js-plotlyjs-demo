package plotlyjs.demo.demo

import org.openmole.plotlyjs.all._
import org.openmole.plotlyjs.PlotlyImplicits._
import org.openmole.plotlyjs._
import com.raquo.laminar.api.L._
import plotlyjs.demo.utils.Utils

import scala.scalajs.js.JSConverters._

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

object LineChartDemo {

  lazy val sc = sourcecode.Text {
    val plotDiv = div()

    val layout = Layout
      .title("My line plot")
      .showlegend(true)
      .xaxis(axis.title("Time"))
      .yaxis(axis.title("Production"))

    val data = linechart.lines

    val ref = Utils.randomDoubles(15, 10)

    val dataRef = data
      .x((0 to 14).toJSArray)
      .y(ref)
      .marker(marker.symbol(square).color(all.color.rgb(180, 0, 0)).size(12.0))
      .name("Reds")


    val dataN = (for (i <- 1 to 6) yield {
      data
        .x((0 to 14).toJSArray)
        .y(ref.map { x => x + Utils.rng.nextDouble * 2 - 1 }.toJSArray)
        .marker(marker.size(10.0).color(all.color.rgb(200, 136, 170)))
        ._result
    }).toJSArray


    val config = Config.displayModeBar(false)

    Plotly.newPlot(plotDiv.ref,
      dataN :+ dataRef._result,
      layout,
      config)

    plotDiv
  }


  val elementDemo = new Demo {
    def title: String = "Line chart"

    def code: String = sc.source

    def element: HtmlElement = sc.value
  }
}
