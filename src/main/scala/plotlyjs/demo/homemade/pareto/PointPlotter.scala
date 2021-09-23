package plotlyjs.demo.homemade.pareto

import plotlyjs.demo.homemade.api.Pareto.{Maximization, Minimization, OptimizationType}
import plotlyjs.demo.homemade.utils.Vectors._

case class PointPlotter(optimizationTypes: Seq[OptimizationType], outputs: Seq[Vector]) {

  val plotOutputs: Seq[Vector] = {
    val orientations = optimizationTypes.map {
        case Maximization => 1.0
        case Minimization => -1.0
    }
    val orientedOutputs = outputs.map(_.mul(orientations))
    val min = orientedOutputs.transpose.map(_.min)
    val max = orientedOutputs.transpose.map(_.max)
    orientedOutputs.map(_.zipWithIndex.map { case (c, i) => (c - min(i))/(max(i) - min(i)) })
  }

}
