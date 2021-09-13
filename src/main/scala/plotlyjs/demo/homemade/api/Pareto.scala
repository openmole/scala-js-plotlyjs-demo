package plotlyjs.demo.homemade.api

import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.html.Div
import plotlyjs.demo.homemade.api.Data.Outcome
import plotlyjs.demo.homemade.pareto.Pareto.plotAPI

object Pareto {

  trait OptimizationType

  object Maximization extends OptimizationType

  object Minimization extends OptimizationType

  case class ParetoObjective(name: String, optimizationType: OptimizationType)

  case class ParetoDisplay(outputPath: Boolean /*TODO rename displayPath, showPath, tracePath, plotPath ?*/, lowerIsBetter: Boolean = false)

  def pareto(objectives: Seq[ParetoObjective], outcomes: Seq[Outcome], paretoDisplay: ParetoDisplay): ReactiveHtmlElement[Div] = plotAPI(objectives, outcomes, paretoDisplay)

}
