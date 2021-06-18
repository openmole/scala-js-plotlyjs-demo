package plotlyjs.demo.demo

import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.Plotly
import org.openmole.plotlyjs.all._
import plotlyjs.demo.utils.Data

import scala.scalajs.js.JSConverters.JSRichIterableOnce

object ParallelCoordinatesDemo {

  private val sc = sourcecode.Text {

    val plotDiv = div()

    val results = Data.dim8Sample100

    val data = parallelCoordinates
      .dimensions(results.transpose.map(values => dimension.values(values.toJSArray)._result).toJSArray)
      ._result

    Plotly.newPlot(plotDiv.ref, Seq(data).toJSArray)

    plotDiv
  }

  val elementDemo: ElementDemo = new ElementDemo{
    def title: String = "Parallel Coordinates"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
