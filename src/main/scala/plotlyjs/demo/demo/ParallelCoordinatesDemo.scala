package plotlyjs.demo.demo

import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.Plotly
import org.openmole.plotlyjs.all._
import plotlyjs.demo.utils.vector.IntVectors._
import plotlyjs.demo.utils.Colors._
import plotlyjs.demo.utils.PointSet.MIN
import plotlyjs.demo.utils.vector.Vectors.{ImplicitScalar, ImplicitVector}
import plotlyjs.demo.utils.{Data, ParetoFront, PointSet, Utils}

import scala.scalajs.js.JSConverters.JSRichIterableOnce

object ParallelCoordinatesDemo {

  private lazy val sc = sourcecode.Text {

    val plotDiv = div()

    val dim = 8
    val results = Utils.randomizeDimensions(new ParetoFront(dim, 128).front)

    val plotData = parallelCoordinates
      .dimensions(
        results.transpose.zipWithIndex.map({ case (values, index) =>
          dimension
            .label("o" + (index + 1))
            .values(values.toJSArray)
            ._result
        }).toJSArray
      )
      ._result

    Plotly.newPlot(plotDiv.ref, Seq(plotData).toJSArray)

    plotDiv
  }

  val elementDemo: Demo = new Demo{
    def title: String = "Parallel Coordinates"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
