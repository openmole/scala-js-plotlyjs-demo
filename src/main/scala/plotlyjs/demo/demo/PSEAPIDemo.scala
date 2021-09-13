package plotlyjs.demo.demo

import com.raquo.laminar.api.L._
import plotlyjs.demo.homemade.api.Data._
import plotlyjs.demo.homemade.api.PSE
import plotlyjs.demo.homemade.api.PSE.PSEDimension
import plotlyjs.demo.utils.vector.Vectors._

import scala.math.random

object PSEAPIDemo {

  private lazy val sc = sourcecode.Text {
    val dimensions = (0 until 4).map(i => PSEDimension("dimension " + i, (0 to 5 + 2*i).map(_.toDouble)))
    val outcomes = (0 until 1024)
      .map(_ => Outcome((0.0 at 4).map(Input("", _)), dimensions.map(d => (d.bounds.last * random()).toInt).map(Output("", _))))
      .distinct
    div(
      PSE.pse(
        dimensions.take(2),
        outcomes.take(8)
      ),
      PSE.pse(
        dimensions.take(3),
        outcomes.take(32)
      ),
      PSE.pse(
        dimensions.take(4),
        outcomes
      )
    )
  }

  val elementDemo: Demo = new Demo {
    def title: String = "PSE API"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
