package plotlyjs.demo.labo

import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.Plotly
import org.openmole.plotlyjs.all.{marker, scatter}
import plotlyjs.demo.demo.Demo
import plotlyjs.demo.utils.ParetoFront
import plotlyjs.demo.utils.Utils.reloadOnDemand

import scala.scalajs.js
import scala.scalajs.js.JSConverters.JSRichIterableOnce

object ParetoFrontGenerationDemo {

  lazy private val sc = sourcecode.Text {
    reloadOnDemand("Random Pareto front", _ => {
      val plotDiv = div()

      val results = ParetoFront.random(2, 128)

      val coordinates = results.transpose
      val data = scatter
        .x(coordinates(0).toJSArray)
        .y(coordinates(1).toJSArray)

      Plotly.plot(plotDiv.ref, js.Array(data))

      plotDiv
    })
  }

  val elementDemo: Demo = new Demo {
    def title: String = "Pareto front generation"

    def code: String = sc.source

    def element: HtmlElement = sc.value
  }

}
