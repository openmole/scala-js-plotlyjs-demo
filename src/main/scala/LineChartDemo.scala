package plotlyjs.demo

import com.definitelyscala.plotlyjs._
import com.definitelyscala.plotlyjs.all._
import com.definitelyscala.plotlyjs.PlotlyImplicits._
import com.definitelyscala.plotlyjs.plotlyConts._
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

      val plotDiv = div.render

      val layout: Layout = Layout
        .title("My line plot")
        .showlegend(true)
        .xaxis(plotlyaxis.title("Time"))
        .yaxis(plotlyaxis.title("Production"))

      val data = PlotData
        .set(plotlymode.markers.lines)
        .set(plotlymarker.set(plotlysymbol.square))

      val data1 = data
        .x(Utils.randomInts(15, 10).sorted)
        .y(Utils.randomInts(15, 10))
        .set(plotlymarker.size(12.0).set(plotlycolor.rgb(180,0,0)))
        .name("Reds")

      val data2 = data
        .x(Utils.randomInts(10, 10).sorted)
        .y(Utils.randomInts(10, 10).sorted)
        .set(plotlymarker.size(10.0).set(plotlycolor.rgb(0, 136, 170)).set(plotlysymbol.cross))
        .name("Blues")

      val config: Config = Config.displayModeBar(false)

      val plot = Plotly.newPlot(plotDiv,
        js.Array(data1, data2),
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
