package plotlyjs.demo

import com.definitelyscala.plotlyjs._
import com.definitelyscala.plotlyjs.all._
import com.definitelyscala.plotlyjs.PlotlyImplicits._
import org.scalajs.dom.raw.Element
import scala.scalajs.js.JSConverters._
import scala.scalajs.js
import scalatags.JsDom.all._

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

  val sc = sourcecode.Text {

    val plotDiv = div.render

    val dimensions = (1 to 5).foldLeft(js.Array[Dimension]()) { (acc, i) =>
      acc :+ Dimension.values(Utils.randomInts(50, 100)).label(s"Dimension $i")._result
    }

    val colorDim = (0 to 100).toJSArray

    val data = PlotData
      .set(plotlytype.splom)
      .set(dimensions)
      .set(plotlymarker
        .set(plotlycolor.array(colorDim))
        .set(ColorScale.hot)
      )

    val config = Config.displayModeBar(false)

    val layout = Layout
      .title("My line plot")
      .height(700)
      .width(700)


    Plotly.newPlot(plotDiv,
      js.Array(data), layout, config)

    div(
      plotDiv,
    ).render
  }


  val elementDemo = new ElementDemo {
    def title: String = "Splom"

    def code: String = sc.source

    def element: Element = sc.value
  }
}