package plotlyjs.demo

import com.definitelyscala.plotlyjs._
import com.definitelyscala.plotlyjs.all._
import scala.scalajs.js.JSConverters._
import com.definitelyscala.plotlyjs.PlotlyImplicits._
import scalatags.JsDom.all._

import scala.scalajs._
import org.scalajs.dom._
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

  import com.definitelyscala.plotlyjs.HistogramDataBuilder._
  val sc = sourcecode.Text {
    val plotDiv = div.render

    val layout = Layout
      .title("My line plot")
      .showlegend(true)
      .xaxis(plotlyaxis.domain(Seq(0,0.4).toJSArray))
      .yaxis(plotlyaxis.anchor("x2"))
      .xaxis2(plotlyaxis.domain(Seq(0.6,1).toJSArray))

    val data1 = histogram
      .x(Utils.randomInts(500))
      .name("First serie")
      .xbins(Bin.start(0.0).end(1000.0).size(25))

    val data2 = histogram
      .x(Utils.anArray.toJSArray.toJSArray)
      .xaxis("x2")
      .yaxis("y2")
      .name("Second serie")

    Plotly.newPlot(plotDiv, js.Array(data1, data2), layout = layout)
    div(plotDiv.render).render
  }


  val elementDemo = new ElementDemo {
    def title: String = "Histogram"

    def code: String = sc.source

    def element: Element = sc.value
  }
}