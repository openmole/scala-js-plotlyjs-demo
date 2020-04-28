package plotlyjs.demo

import com.definitelyscala.plotlyjs
import com.definitelyscala.plotlyjs._
import com.definitelyscala.plotlyjs.all._
import com.definitelyscala.plotlyjs.PlotlyImplicits._
import com.definitelyscala.plotlyjs.PlotlyStatic.DatumArray

import scala.scalajs.js.JSConverters._
import org.scalajs.dom.raw.Element
import scalatags.JsDom.all._

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

object LineChartDemo {

  val sc = sourcecode.Text {

    import plotlyjs.PlotlyStatic

    val plotDiv = div.render

    val layout = Layout
      .title("My line plot")
      .showlegend(true)
      .xaxis(plotlyaxis.title("Time"))
      .yaxis(plotlyaxis.title("Production"))

    val data = linechart.lines

    val ref = Utils.randomDoubles(15, 10)


    val dataRef = data
      .x((0 to 14).toJSArray)
      .y(ref)
      .marker(plotlymarker.symbol(square).color(plotlycolor.rgb(180, 0, 0)).size(12.0))
      .name("Reds")


    val dataN = (for (i <- 1 to 6) yield {
      data
        .x((0 to 14).toJSArray)
        .y(ref.map { x => x + Utils.rng.nextDouble * 2 - 1 }.toJSArray)
        .marker(plotlymarker.size(10.0).color(plotlycolor.rgb(200, 136, 170)))
        ._result
    }).toJSArray


    val config = Config.displayModeBar(false)

    Plotly.newPlot(plotDiv,
      dataN :+ dataRef._result,
      layout,
      config)

    div(plotDiv.render).render
  }


  val elementDemo = new ElementDemo {
    def title: String = "Line chart"

    def code: String = sc.source

    def element: Element = sc.value
  }
}
