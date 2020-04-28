package plotlyjs.demo

import com.definitelyscala.plotlyjs._
import com.definitelyscala.plotlyjs.all._
import com.definitelyscala.plotlyjs.PlotlyImplicits._
import com.definitelyscala.plotlyjs.plotlyConts._
import org.querki.jsext.JSOptionBuilder
import org.scalajs.dom.raw.Element

import scalatags.JsDom.all._
import scala.scalajs._
import org.scalajs.dom._
import scaladget.tools.JsRxTags._
import rx._
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

  val sc = sourcecode.Text {
    val clickText = Var("")

    val plotDiv = div.render

    val layout = Layout
      .title("My box plot")
      .showlegend(true)

    val data1 = box
      .y(Utils.randomInts(50, 50))
      .set(plotlymarker.set(plotlycolor.rgba(180,180,0,0.5))
        .set(plotlysizemode.area))
      .name("First set")

    val data2 = box
      .y(Utils.randomInts(50, 40))
      .set(plotlymarker.set(plotlycolor.rgba(180,0,180,1))
        .set(plotlysizemode.area))
      .name("Second set")

    val config = Config.displayModeBar(false)
    Plotly.newPlot(plotDiv, js.Array(data1, data2), layout, config)

    plotDiv.on(PlotEvent.CLICK, (d: PointsData) => {
      clickText() = d.points.map { p => s"${p.y}" }.mkString(" , ")
    })

    div(
      plotDiv,
      div(Rx {
        clickText()
      })
    ).render
    //div(plotDiv.render).render
  }


  val elementDemo = new ElementDemo {
    def title: String = "Box"

    def code: String = sc.source

    def element: Element = sc.value
  }
}
