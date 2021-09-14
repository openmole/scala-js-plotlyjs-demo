package plotlyjs.demo.homemade.pareto

import plotlyjs.demo.homemade.api.Pareto.{Maximization, Minimization, OptimizationType}
import plotlyjs.demo.homemade.pareto.PointPlotter._
import plotlyjs.demo.homemade.utils.Vectors._

case class PointPlotter(optimizationTypes: Seq[OptimizationType], outputs: Seq[Vector], betterPlot: BetterPlot) {

  val plotOutputs: Seq[Vector] = {
    val orientations = optimizationTypes.map((_, betterPlot)).map {
        case (Minimization, BetterPlot.IsLower) | (Maximization, BetterPlot.IsHigher) => 1.0
        case (Minimization, BetterPlot.IsHigher) | (Maximization, BetterPlot.IsLower) => -1.0
    }
    val orientedOutputs = outputs.map(_.mul(orientations))
    val min = orientedOutputs.transpose.map(_.min)
    val max = orientedOutputs.transpose.map(_.max)
    orientedOutputs.map(_.zipWithIndex.map { case (c, i) => (c - min(i))/(max(i) - min(i)) })
  }

}

object PointPlotter {

  class BetterPlot
  object BetterPlot {
    object IsLower extends BetterPlot
    object IsHigher extends BetterPlot
  }

}
