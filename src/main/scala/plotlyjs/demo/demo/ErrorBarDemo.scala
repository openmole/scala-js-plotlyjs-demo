package plotlyjs.demo.demo

import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import org.openmole.plotlyjs.PlotlyImplicits._

import scala.scalajs.js.JSConverters._
import com.raquo.laminar.api.L._
import plotlyjs.demo.utils.Utils

import scala.scalajs._

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

object ErrorBarDemo {

  val sc = sourcecode.Text {

    val plotDiv = div()

    val layout = Layout
      .title("My line plot")
      .showlegend(true)
      .xaxis(axis.title("Time"))
      .yaxis(axis.title("Production"))

    val data = linechart.lines

    val data1 = data
      .x((0 to 14).toJSArray)
      .y(Utils.randomDoubles(15, 10))
      .errorY(ErrorY.array(Utils.randomDoubles(15,5)))
      .marker(marker.size(12.0).color(all.color.rgb(180,0,0)).symbol(square))
      .name("Reds")


    val config = Config.displayModeBar(false)

    Plotly.newPlot(plotDiv.ref,
      js.Array(data1),
      layout,
      config)

    plotDiv
  }


  val elementDemo = new ElementDemo {
    def title: String = "Error bar"

    def code: String = sc.source

    def element: HtmlElement = sc.value
  }
}
