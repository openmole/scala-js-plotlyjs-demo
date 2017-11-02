package plotlyjs.demo

package demo

import scaladget.api.{JSDependency, BootstrapTags => bs}
import scaladget.stylesheet.{all => sheet}
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom

import scalatags.JsDom.tags
import scalatags.JsDom.all._
import sheet._
import bs._

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
      LineChartDemo.elementDemo,
      HistogramDemo.elementDemo,
      ScatterDemo.elementDemo
    )

    JSDependency.withJS(JSDependency.BOOTSTRAP_NATIVE) {

      val tabs = demos.foldLeft(bs.tabs) { (acc, demo) =>
        acc.add(demo.title,
          div(marginLeft := 15, marginTop := 25)(
            h3(demo.title),
            div(row)(
              div(colMD(demo.codeWidth))(pre(code(toClass("scala"))(demo.cleanCode))),
              div(colMD(12 - demo.codeWidth))(demo.element)
//              div(colMD(8))(pre(code(toClass("scala"))(s"/*${imports}\n\n*/${demo.cleanCode}")), width := "75%"),
//              div(colMD(4))(demo.element)
            )
          )
        )
      }
      div(padding := 20)(
        tabs.render(sheet.pills)
      ).render
    }
    dom.document.body.appendChild(tags.script("hljs.initHighlighting();"))
  }
}
