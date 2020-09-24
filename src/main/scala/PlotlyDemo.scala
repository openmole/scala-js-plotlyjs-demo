package plotlyjs.demo

package demo

import scaladget.bootstrapnative.bsn._
import scaladget.bootstrapnative.bsnsheet
import scaladget.tools._
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom

import scalatags.JsDom.tags
import scalatags.JsDom.all._

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
object PlotlyDemo {

  @JSExportTopLevel("plotlyDemo")
  def plotlyDemo(): Unit = {

    def imports =
      """
    import com.definitelyscala.plotlyjs._
    import com.definitelyscala.plotlyjs.all._
    import com.definitelyscala.plotlyjs.PlotlyImplicits._
    import com.definitelyscala.plotlyjs.plotlyConts._
    import scala.scalajs.js.JSConverters._

    import scalatags.JsDom.all._
    import scala.scalajs._
    import org.scalajs.dom._
      """.stripMargin

    val demos = Seq(
      ScatterPolarDemo.elementDemo,
      LineChartDemo.elementDemo,
      HistogramDemo.elementDemo,
      ScatterDemo.elementDemo,
      BoxDemo.elementDemo,
      SplomDemo.elementDemo,
      HeatMapDemo.elementDemo,
      ErrorBarDemo.elementDemo
    )

    val tabs = demos.foldLeft(Tabs.tabs()) { (acc, demo) =>
      acc.add(demo.title,
        div(marginLeft := 15, marginTop := 25)(
          h3(demo.title),
          div(row)(
            div(colMD(demo.codeWidth))(pre(code(toClass("scala"))(demo.cleanCode))),
            div(colMD(12 - demo.codeWidth))(demo.element)
          )
        )
      )
    }

    dom.document.body.appendChild(
      div(padding := 20)(
        tabs.build.render(bsnsheet.pills)
      ).render
    )

    dom.document.body.appendChild(tags.script("hljs.initHighlighting();"))
  }
}
