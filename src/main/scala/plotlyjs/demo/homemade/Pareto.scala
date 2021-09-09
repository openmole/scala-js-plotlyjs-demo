package plotlyjs.demo.homemade

import plotlyjs.demo.homemade.Data.Outcome

object Pareto {

  trait OptimizationType
  object Maximization extends OptimizationType
  object Minimization extends OptimizationType

  case class ParetoObjective(name: String, optimizationType: OptimizationType)

  case class ParetoDisplay(outputPath: Boolean, compromiseHelp: Boolean)

  def pareto(objectives: Seq[ParetoObjective], outcomes: Seq[Outcome], paretoDisplay: ParetoDisplay) = ???

}
