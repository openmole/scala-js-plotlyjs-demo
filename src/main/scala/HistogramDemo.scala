package plotlyjs.demo

import com.definitelyscala.plotlyjs._

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

  val sc = sourcecode.Text {
    val plotDiv = div.render

    val histogramData = PlotData.`type`("histogram")

    val data1 = histogramData
      .x(Utils.randomInts(500))

    val data2 = histogramData
      .x(Utils.randomInts(500))

    Plotly.newPlot(plotDiv, js.Array(data1, data2))
    div(plotDiv.render).render
  }


  val elementDemo = new ElementDemo {
    def title: String = "Histogram"

    def code: String = sc.source

    def element: Element = sc.value
  }
}