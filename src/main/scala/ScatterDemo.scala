package plotlyjs.demo
import com.definitelyscala.plotlyjs._
import com.definitelyscala.plotlyjs.all._
import com.definitelyscala.plotlyjs.PlotlyImplicits._
import com.definitelyscala.plotlyjs.plotlyConts._
import org.scalajs.dom.raw.Element

import scala.scalajs.js
import scalatags.JsDom.all._
import scaladget.tools.JsRxTags._
import rx._

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

      val plotDiv = div.render

      val colorDim = Utils.randomDoubles()

      val data = PlotData
        .x(Utils.randomDoubles())
        .y(Utils.randomDoubles())
        .customdata(colorDim.map{_.toString})
        .mode(PlotMode.MARKERS)
          .`type`("scatter")
        .set(plotlymarker
            .set(plotlysizemode.area)
            .size(colorDim)
            .set(plotlycolor.array(colorDim))
            .set(plotlycolorscale.jet)
        )

      val config: Config = Config.displayModeBar(false)
      Plotly.plot(plotDiv, js.Array(data), config = config)


      plotDiv.on(PlotEvent.HOVER, (d: PointsData) => {
        hoverText() = d.points.map { p => s"${p.x} ${p.y} ${p.customdata}" }.mkString(" and ")
      })

      div(
        plotDiv,
        div(Rx {
          hoverText()
        })
      ).render
    }


  val elementDemo = new ElementDemo {
    def title: String = "Scatter"

    def code: String = sc.source

    def element: Element = sc.value
  }

  }