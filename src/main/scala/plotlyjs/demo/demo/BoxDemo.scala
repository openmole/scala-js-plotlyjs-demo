package plotlyjs.demo.demo

import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import org.openmole.plotlyjs.PlotlyImplicits._
import org.openmole.plotlyjs.plotlyConts._
import org.scalajs.dom.raw.Element
import com.raquo.laminar.api.L._
import plotlyjs.demo.utils.Utils

import scala.scalajs._
/*
 * Copyright (C) 31/10/17 // mathieu.leclaire@openmole.org
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

object BoxDemo {

  lazy val sc = sourcecode.Text {
    val clickText = Var("")

    val plotDiv = div()

    val layout = Layout
      .title("My box plot")
      .showlegend(true)

    val data1 = box
      .y(Utils.randomInts(50, 50))
      .marker(marker
        .color(all.color.rgba(180,180,0,0.5))
        .sizeMode(sizemode.area)
      )
      .name("First set")

    val data2 = box
      .y(Utils.randomInts(50, 40))
      .marker(marker
        .color(all.color.rgba(180,0,180,1))
        .sizeMode(sizemode.area))
      .name("Second set")

    val config = Config.displayModeBar(false)
    Plotly.newPlot(plotDiv.ref, js.Array(data1, data2), layout, config)

    plotDiv.ref.on(PlotEvent.CLICK, (d: PointsData) => {
      clickText.set(d.points.map { p => s"${p.y}" }.mkString(" , "))
    })

    div(
      plotDiv,
      child.text <-- clickText.signal
    )
  }


  val elementDemo = new Demo {
    def title: String = "Box"

    def code: String = sc.source

    def element: HtmlElement = sc.value
  }
}
