package plotlyjs.demo

import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import org.openmole.plotlyjs.PlotlyImplicits._
import org.openmole.plotlyjs.plotlyConts._

import scala.scalajs.js
import com.raquo.laminar.api.L._

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
object ScatterDemo {

  val sc = sourcecode.Text {
    val hoverText = Var("")

    val plotDiv = div()

    val colorDim = Utils.randomDoubles()

    val data = scatter
      .x(Utils.randomDoubles())
      .y(Utils.randomDoubles())
      .customdata(colorDim.map {
        _.toString
      })
      .marker(marker
        .sizeMode(sizemode.area)
        .size(12)
        .color(org.openmole.plotlyjs.all.color.rgba(60, 90, 140, 0.5))
        .symbol(circle)
        .line(line.color(org.openmole.plotlyjs.all.color.rgb(60, 90, 140)).width(2))
      )

    val config = Config.displayModeBar(false)
    Plotly.newPlot(plotDiv.ref, js.Array(data), config = config)


    plotDiv.ref.on(PlotEvent.HOVER, (d: PointsData) => {
      hoverText.set(d.points.map { p => s"${p.x} ${p.y} ${p.customdata}" }.mkString(" and "))
    })

    div(
      plotDiv,
      child.text <-- hoverText.signal
    )
  }


  val elementDemo = new ElementDemo {
    def title: String = "Scatter"

    def code: String = sc.source

    def element: HtmlElement = sc.value
  }

}
