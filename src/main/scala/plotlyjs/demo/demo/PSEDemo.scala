
package plotlyjs.demo.demo


import org.openmole.plotlyjs.PlotMode.markers
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import org.openmole.plotlyjs.PlotlyImplicits._

import scala.scalajs.js.JSConverters._
import scala.scalajs.js
import com.raquo.laminar.api.L._
import plotlyjs.demo.utils.Data

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

object PSEDemo {

  import org.openmole.plotlyjs.ScatterTernaryDataBuilder._

  val sc = sourcecode.Text {

    val plotDiv = div()

    val data = scatterTernary.
      a(Data.idealPSE("area").map{_._1}.toJSArray).
      b(Data.idealPSE("compacity").map{_._2}.toJSArray).
      c(Data.idealPSE("convexity").map{_._3}.toJSArray).
      text(js.Array(Data.area, Data.compacity, Data.convexity)).
      textPosition(TextPosition.topCenter).
      setMode(markers).set(
      marker.size(5).color(Color.rgb(122, 122, 122)).symbol(circle).opacity(0.5)
    )._result

    val values = Data.pse.map{p=>
      if (p.dimension.name == "area") p.values.map{_/10000}
      else p.values
    }

    val data2 = scatterTernary.
      a(values(0).toJSArray).
      b(values(1).toJSArray).
      c(values(2).toJSArray).
      text(js.Array("area", "compacity", "convexity")).
      textPosition(TextPosition.topCenter).
      setMode(markers).set(
      marker.size(8).color(Color.rgb(122, 0, 0)).symbol(circle)
    )._result

    val data3 = scatterTernary.
      a(js.Array(0.1).toJSArray).
      b(js.Array(0.1).toJSArray).
      c(js.Array(0.1).toJSArray).
      text(js.Array("area", "compacity", "convexity")).
      textPosition(TextPosition.topCenter).
      setMode(markers).set(
      marker.size(15).color(Color.rgb(0, 0, 122)).symbol(circle)
    )._result

    val layout = Layout.ternary(
      ternary
      //  .sum(100)
        .aaxis(axis.dtick(0.1).title("Area"))
        .baxis(axis.dtick(0.1).title("Compacity"))
        .caxis(axis.dtick(0.1).title("Convexity"))
    ).width(800).height(800)

    Plotly.newPlot(plotDiv.ref, Array(data, data2).toJSArray, layout)

    plotDiv

  }

  val elementDemo = new ElementDemo {
    def title: String = "PSE"

    def code: String = sc.source

    def element: HtmlElement = sc.value
  }

}