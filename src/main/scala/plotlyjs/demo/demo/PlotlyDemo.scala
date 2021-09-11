package plotlyjs.demo.demo

import scaladget.highlightjs.HighlightJS
import scaladget.bootstrapnative.bsn._
import com.raquo.laminar.api.L._
import org.scalajs
import plotlyjs.demo.labo.{DirectionsScatterDemo, HyperplaneSubdivisionDemo, PSEDemo, PSEMultiScaleDemo, PSESVGDemo, PSESubPlotsDemo, Pareto3dDemo, ParetoBisDemo, ParetoDemo, ParetoFrontGenerationDemo}

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

  def main(argv: Array[String]): Unit = {

    scaladget.highlightjs.scalamode
    HighlightJS.initHighlightingOnLoad()

    def imports =
      """
    import com.definitelyscala.plotlyjs._
    import com.definitelyscala.plotlyjs.all._
    import com.definitelyscala.plotlyjs.PlotlyImplicits._
    import com.definitelyscala.plotlyjs.plotlyConts._
    import scala.scalajs.js.JSConverters._

    import com.raquo.laminar.api.L._
    import scala.scalajs._
      """.stripMargin


    lazy val content =
      div(containerFluid, marginLeft := "15", marginTop := "25",
        h3("Build"),
        div(row,
          div(colSM, "Details on construction on ", a(href := "https://github.com/openmole/scala-js-plotlyjs", target := "_blank", "the scala-js-plotlyjs facade Github page"))
        ),
        h3("Imports"),
        div(colSM, pre(code(cls("scala"), imports))),
        Tabs.tabs(
          for {
            elementDemo <- Seq(
              LineChartDemo.elementDemo,
              HistogramDemo.elementDemo,
              ScatterDemo.elementDemo,
              BoxDemo.elementDemo,
              SplomDemo.elementDemo,
              HeatMapDemo.elementDemo,
              ErrorBarDemo.elementDemo,
              ParallelCoordinatesDemo.elementDemo,
              PSEAPIDemo.elementDemo,

              PSESubPlotsDemo.elementDemo,
              PSESVGDemo.elementDemo,
              PSEDemo.elementDemo,
              PSEMultiScaleDemo.elementDemo,
              ParetoDemo.elementDemo,
              ParetoBisDemo.elementDemo,
              Pareto3dDemo.elementDemo,
              //
              DirectionsScatterDemo.elementDemo,
              ParetoFrontGenerationDemo.elementDemo,
              //RegularDirectionsDemo.elementDemo,
              //RegularDirectionsWithCacheDemo.elementDemo,
              //RestrictedSpaceTransformationDemo.elementDemo,
              //ScatterPPSEDemo.elementDemo,
              HyperplaneSubdivisionDemo.elementDemo,
            )
          } yield {
            lazy val demoDiv = {
              div(
                h3(elementDemo.title),
                div(containerFluid,
                  div(row, marginLeft := "15", marginTop := "25",
                    div(colBS(elementDemo.codeWidth), pre(code(cls := "scala", elementDemo.cleanCode))),
                    div(colBS(12 - elementDemo.codeWidth), elementDemo.element)
                  )
                )
              )
            }
            if(elementDemo.isLazy) {
              val demoVar = Var(div("Default value"))
              //demoVar.set()
              //val loadingDiv = div("Loading message test", onMountInsert { _ => demoDiv} /*inContext( { _ => onLoad.mapTo(demoDiv) --> demoVar.writer})*/)
              val tabContainer = div(/*name := "tabContainer", */child <-- demoVar.signal)
              Tab(elementDemo.title,
                tabContainer,
                //() => { render(scalajs.dom.document.body/*.children.namedItem("tabContainer")*/, demoDiv)/*demoVar.set(/*loadingDiv*/demoDiv)*/ }
                () => { demoVar.set(demoDiv) }
              )
            } else {
              Tab(elementDemo.title, demoDiv)
            }
          },
          tabStyle = navbar_pills
        ).build.render
      )

    documentEvents.onDomContentLoaded.foreach { _ =>
      render(scalajs.dom.document.body, content)
    }(unsafeWindowOwner)
    // dom.document.body.appendChild(tags.script("hljs.initHighlighting();"))
  }
}
