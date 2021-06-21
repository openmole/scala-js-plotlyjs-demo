package plotlyjs.demo.utils

class PointSet(val rawOutputs: Seq[Seq[Double]]) {

  //val inputDimension: Int
  val outputDimension: Int = rawOutputs.head.length
  val size: Int = rawOutputs.size

  //var rawInputs: Array[Array[Double]]
  //var plotInputs: Array[Array[Double]]

  private def multiply(vector: Seq[Double], scalar: Double): Seq[Double] = vector.map(_ * scalar)
  private def multiply(vector1: Seq[Double], vector2: Seq[Double]): Seq[Double] = vector1.zip(vector2) map { case (c1, c2) => c1 * c2 }
  private def multiplyAll(vectors: Seq[Seq[Double]], vector: Seq[Double]): Seq[Seq[Double]] = vectors.map(multiply(_, vector))

  private var _optimizationProblems: Seq[Double] = Array.fill[Double](outputDimension)(0)
  def optimizationProblems(optimizationProblems: Seq[Double]): PointSet = {
    _optimizationProblems = optimizationProblems
    this
  }
  private var _orientation = 0
  def higherPlotIsBetter: PointSet = {
    _orientation = 1
    this
  }
  def lowerPlotIsBetter: PointSet = {
    _orientation = -1
    this
  }
  private lazy val _orientedOutputs: Seq[Seq[Double]] = multiplyAll(rawOutputs, multiply(_optimizationProblems, _orientation))
  lazy val spaceNormalizedOutputs: Seq[Seq[Double]] = {
    val min = _orientedOutputs.transpose.map(_.min)
    val max = _orientedOutputs.transpose.map(_.max)
    _orientedOutputs.map(_.zipWithIndex.map { case (c, i) => (c - min(i))/(max(i) - min(i)) })
  }

  lazy val norm1VectorNormalizedOutputs: Seq[Seq[Double]] = spaceNormalizedOutputs.map(p => {
    val norm1 = p.map(math.abs).sum
    if(norm1 == 0) p else multiply(p, 1/norm1)
  })

}

object PointSet {
  val MAX: Double = 1
  val MIN: Double = -1
}
