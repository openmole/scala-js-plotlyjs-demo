package plotlyjs.demo.homemade

import plotlyjs.demo.homemade.Data.Outcome

object PSE {

  case class PSEObjective(name: String, bounds: Seq[Double])

  def pse(objectives: Seq[PSEObjective], outcomes: Seq[Outcome]) = ???

}
