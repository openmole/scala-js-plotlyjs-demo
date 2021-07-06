package plotlyjs.demo.demo

import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import org.openmole.plotlyjs.PlotlyImplicits._
import org.scalajs.dom.raw.Element

import scala.scalajs.js.JSConverters._
import scala.scalajs.js
import com.raquo.laminar.api.L._
import plotlyjs.demo.utils.Utils

/*
 * Copyright (C) 17/10/18 // mathieu.leclaire@openmole.org
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


object SplomDemo {

  lazy val sc = sourcecode.Text {

    import org.openmole.plotlyjs.SplomDataBuilder._
    import org.openmole.plotlyjs.Layout._

    val sup = 100
    val size = 50

    val plotDiv = div()

    val dimensions = (1 to 5).foldLeft(js.Array[Dimension]()) { (acc, i) =>
      acc :+ Dimension.values(Utils.randomInts(size, sup)).label(s"Dimension $i")._result
    }

    val colors = (0 to size).toJSArray map { x => x.toDouble / size }

    val data = splom
      .set(dimensions)
      .showupperhalf(false)
      .diagonalOff
      .marker(marker
        .color(Color.array(colors))
        .showscale(true)
      )

    val config = Config.displayModeBar(false)

    val layout = Layout
      .title("My line plot")
      .dragmode(DragMode.select)
      .hovermode(HoverMode.yUnified)
      .height(700)
      .width(700)


    Plotly.newPlot(plotDiv.ref,
      js.Array(data), layout, config)

    plotDiv
  }


  val elementDemo = new ElementDemo {
    def title: String = "Splom"

    def code: String = sc.source

    def element: HtmlElement = sc.value
  }
}