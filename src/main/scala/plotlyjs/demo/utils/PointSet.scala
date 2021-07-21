package plotlyjs.demo.utils

import plotlyjs.demo.utils.Vectors._

class PointSet(val rawOutputs: Seq[Vector]) {

  //val inputDimension: Int
  val outputDimension: Int = rawOutputs.head.length
  val size: Int = rawOutputs.size

  //var rawInputs: Array[Array[Double]]
  //var plotInputs: Array[Array[Double]]

  private var _optimizationProblems: Seq[Double] = 1 at outputDimension
  def optimizationProblems(optimizationProblems: Seq[Double]): PointSet = {
    _optimizationProblems = optimizationProblems
    this
  }

  private var _orientation = 1
  def higherPlotIsBetter: PointSet = {
    _orientation = 1
    this
  }
  def lowerPlotIsBetter: PointSet = {
    _orientation = -1
    this
  }

  private lazy val _orientedOutputs: Seq[Vector] = rawOutputs.map(mul(_orientation * _optimizationProblems))

  lazy val spaceNormalizedOutputs: Seq[Vector] = {
    val min = _orientedOutputs.transpose.map(_.min)
    val max = _orientedOutputs.transpose.map(_.max)
    _orientedOutputs.map(_.zipWithIndex.map { case (c, i) => (c - min(i))/(max(i) - min(i)) })
  }

  lazy val norm1VectorNormalizedOutputs: Seq[Vector] = spaceNormalizedOutputs.map(normalize(1))

  class PointSetSlice(from: Int, until: Int) {

    def slice[A](seq: Seq[A]): Seq[A] = seq.slice(from, until)

    lazy val rawOutputs: Seq[Vector] = slice(PointSet.this.rawOutputs)
    val outputDimension: Int = PointSet.this.outputDimension
    val pointSetSize: Int = PointSet.this.size
    val sliceSize: Int = until - from
    lazy val spaceNormalizedOutputs: Seq[Vector] = slice(PointSet.this.spaceNormalizedOutputs)
    lazy val norm1VectorNormalizedOutputs: Seq[Vector] = slice(PointSet.this.norm1VectorNormalizedOutputs)
  }

}

object PointSet {
  val MAX: Double = 1
  val MIN: Double = -1
}
