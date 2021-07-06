package plotlyjs.demo.demo

import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import org.openmole.plotlyjs.PlotlyImplicits._

import scala.scalajs.js
import com.raquo.laminar.api.L._
import plotlyjs.demo.utils.Utils

/*
 * Copyright (C) 31/10/18 // mathieu.leclaire@openmole.org
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


object HeatMapDemo {

  lazy val sc = sourcecode.Text {

    val plotDiv = div()

    val data1 = heatmap
      .z((1 to 100).foldLeft(js.Array[js.Array[Int]]())((acc, i) => acc :+ Utils.randomInts(50, 100 * i)))


    val layout = Layout
      .title("My heat map")
      .height(700)
      .width(700)


    Plotly.newPlot(plotDiv.ref,
      js.Array(data1))

    plotDiv
  }


  val elementDemo = new ElementDemo {
    def title: String = "Heat map"

    def code: String = sc.source

    def element: HtmlElement = sc.value
  }
}