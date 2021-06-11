package plotlyjs.demo

class PointSet(val rawOutputs: Seq[Seq[Double]]) {

  //val inputDimension: Int
  val outputDimension: Int = rawOutputs.head.length
  val size: Int = rawOutputs.size

  //var rawInputs: Array[Array[Double]]
  //var plotInputs: Array[Array[Double]]
  private var _plotOutputs: Seq[Seq[Double]] = rawOutputs

  private var _optimizationProblems: Seq[Double] = Array.fill[Double](outputDimension)(0)

  def plotOutputs: Seq[Seq[Double]] = _plotOutputs

  def optimizationProblems(optimizationProblems: Seq[Double]): PointSet = {
    _optimizationProblems = optimizationProblems
    this
  }

  private def multiply(vector: Seq[Double], scalar: Double): Seq[Double] = vector.map(_ * scalar)
  private def multiply(vector1: Seq[Double], vector2: Seq[Double]): Seq[Double] = vector1.zip(vector2) map {case (c1, c2) => c1 * c2}
  private def multiplyAll(vectors: Seq[Seq[Double]], vector: Seq[Double]): Seq[Seq[Double]] = vectors.map(multiply(_, vector))

  def higherPlotIsBetter: PointSet = {
    _plotOutputs = multiplyAll(rawOutputs, _optimizationProblems)
    this
  }

  def lowerPlotIsBetter: PointSet = {
    _plotOutputs = multiplyAll(rawOutputs, multiply(_optimizationProblems, -1))
    this
  }

  def normalizePlotOutputSpace: PointSet = {
    val min = _plotOutputs.transpose.map(_.min)
    val max = _plotOutputs.transpose.map(_.max)
    _plotOutputs = _plotOutputs.map(p => for((c, i) <- p.zipWithIndex) yield (c - min(i))/(max(i) - min(i)))
    this
  }

  def normalizePlotOutputPointsNorm1: PointSet = {
    _plotOutputs = _plotOutputs.map(p => {
      val norm1 = p.map(math.abs).sum
      if(norm1 == 0) p else multiply(p, 1/norm1)
    })
    this
  }

  def slice(from: Int, until: Int): PointSet = {
    val pointSet = new PointSet(rawOutputs.slice(from, until))
      .optimizationProblems(_optimizationProblems.slice(from, until))
    pointSet._plotOutputs = _plotOutputs.slice(from, until)
    pointSet
  }

}

object PointSet {
  val MAX: Double = 1
  val MIN: Double = -1
}
