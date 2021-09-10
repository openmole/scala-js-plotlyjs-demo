package plotlyjs.demo.homemade.pse

import com.raquo.laminar.api.L._
import plotlyjs.demo.demo.Demo
import plotlyjs.demo.homemade.api.Data._
import plotlyjs.demo.homemade.api.PSE
import plotlyjs.demo.homemade.api.PSE._
import plotlyjs.demo.utils.vector.Vectors._

import scala.math.random

object PSEAPIDemo {

  private lazy val sc = sourcecode.Text {
    PSE.pse(
      (0 until 4).map(_ => PSEDimension("", (0 to 5).map(_.toDouble))),
      (0 until 128).map(_ => Outcome((0.0 at 4).map(Input("", _)), ((() => random() * 5) at 4).map(Output("", _)))) //TODO distinct
    )
  }

  val elementDemo: Demo = new Demo {
    def title: String = "PSE API"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
