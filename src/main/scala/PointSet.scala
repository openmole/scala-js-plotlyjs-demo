package plotlyjs.demo

class PointSet(val rawOutputs: Seq[Seq[Double]]) {

  //val inputDimension: Int
  val outputDimension: Int = rawOutputs.head.length
  //val size = rawOutputs.size

  //var rawInputs: Array[Array[Double]]
  //var plotInputs: Array[Array[Double]]
  private var plotOutputs: Seq[Seq[Double]] = Seq[Seq[Double]]()

  private var optimizationProblems: Seq[Double] = Array.fill[Double](outputDimension)(0)

  def getPlotOutputs: Seq[Seq[Double]] = {
    //println(s"plotOutputs = ${plotOutputs}")
    plotOutputs
  }

  def setOptimizationProblems(optimizationProblems: Seq[Double]): PointSet = {
    this.optimizationProblems = optimizationProblems
    this
  }

  private def multiply(vector: Seq[Double], scalar: Double): Seq[Double] = vector.map(_ * scalar)
  private def multiply(vector1: Seq[Double], vector2: Seq[Double]): Seq[Double] = for((c, i) <- vector1.zipWithIndex) yield c * vector2(i)
  private def multiplyAll(vectors: Seq[Seq[Double]], vector: Seq[Double]): Seq[Seq[Double]] = vectors.map(multiply(_, vector))

  def higherPlotIsBetter: PointSet = {
    plotOutputs = multiplyAll(rawOutputs, optimizationProblems)
    this
  }

  def lowerPlotIsBetter: PointSet = {
    plotOutputs = multiplyAll(rawOutputs, multiply(optimizationProblems, -1))
    this
  }

  def normalizePlotOutputSpace: PointSet = {
    val min = Array.ofDim[Double](outputDimension)
    val max = Array.ofDim[Double](outputDimension)
    for(p <- plotOutputs) for((c, i) <- p.zipWithIndex) {
      min(i) = Math.min(min(i), c)
      max(i) = Math.max(max(i), c)
    }
    plotOutputs = plotOutputs.map(p => for((c, i) <- p.zipWithIndex) yield (c - min(i))/(max(i) - min(i)))
    this
  }

  /*
  object OptimizationProblem extends Enumeration {
    type OptimizationProblem = Value
    val max, min = Value
  }
  */

  /*
  def set(index: Int, dimension: Int, value: Double) = points(index)(dimension) = value

  def optimizationProblems(ops: Seq[OptimizationProblem.OptimizationProblem]) = {
    this.ops = ops
    this
  }

  def plotTo(op: OptimizationProblem.OptimizationProblem) = {

  }
  */
}

object PointSet {
  val MAX: Double = 1
  val MIN: Double = -1
}
