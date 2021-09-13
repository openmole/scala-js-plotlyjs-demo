package plotlyjs.demo.homemade.api

import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.html.Div
import plotlyjs.demo.homemade.api.Data.Outcome
import plotlyjs.demo.homemade.pse.PSE.plotAPI

object PSE {

  case class PSEDimension(name: String, bounds: Seq[Double])

  def pse(dimensions: Seq[PSEDimension], outcomes: Seq[Outcome]): ReactiveHtmlElement[Div] = plotAPI(dimensions, outcomes)

}
