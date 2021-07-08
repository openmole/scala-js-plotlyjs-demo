package plotlyjs.demo.demo

import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._

import scala.scalajs.js.JSConverters._
import org.openmole.plotlyjs.PlotlyImplicits._
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

object HistogramDemo {

  import org.openmole.plotlyjs.HistogramDataBuilder._
  lazy val sc = sourcecode.Text {
    val plotDiv = div()

    val layout = Layout
      .title("My histogram")
      .grid(grid.columns(3).rows(1).pattern(Pattern.coupled))
      .showlegend(true)

    val data1 = histogram
      .x(Utils.randomInts(500))
      .name("First serie")
      .xbins(Bin.start(0.0).end(1000.0).size(25))

    val data2 = histogram
      .x(Utils.anArray.toJSArray)
      .xaxis("x2")
      .name("Second serie")

    val data3 = histogram
      .x(Utils.anArray.toJSArray)
      .xaxis("x3")
      .name("Second serie")

    Plotly.newPlot(plotDiv.ref, js.Array(data1, data2, data3), layout = layout)

    plotDiv
  }


  val elementDemo = new ElementDemo {
    def title: String = "Histogram"

    def code: String = sc.source

    def element: HtmlElement = sc.value
  }
}