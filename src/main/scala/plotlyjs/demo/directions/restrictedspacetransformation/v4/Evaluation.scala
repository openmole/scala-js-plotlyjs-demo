package plotlyjs.demo.directions.restrictedspacetransformation.v4

import plotlyjs.demo.utils.vector.IntVectors._
import plotlyjs.demo.directions.restrictedspacetransformation.v4.IndexedTransformation.{circle, circleWithIndex, fromCircleToIndex, fromIndexToCircle, neighbourhood}
import plotlyjs.demo.directions.restrictedspacetransformation.v4.Transformation.fromSquareToCircle
import plotlyjs.demo.utils.vector.Vectors._

import scala.math.{pow, sqrt}

object Evaluation {

  def lossEvaluation(maxDimension: Int, radius: Int): Unit = {
    for(dimension <- 3 to maxDimension) {
      val size = circle(dimension, radius).size.toDouble
      val cubeSize = (2 * dimension * pow(2 * radius + 1, dimension - 1))
      val loss = 1 - size/cubeSize
      println(dimension, size, cubeSize, loss)
    }
  }

  def evaluationByNeighbourhood(dimension: Int, radius: Int, noTransform: Boolean = false): Iterable[Seq[Double]] = {
    println("Evaluation")
    circleWithIndex(dimension, radius)
      .map { case (circleVector, indexVector) => (circleVector, neighbourhood(indexVector).flatMap(fromIndexToCircle)) }
      .filter(_._2.nonEmpty)
      .flatMap { case (circleVector, neighbourhood) =>
        neighbourhood.map(neighbour => {
          Seq(fromCircleToIndex(neighbour).vector ^ fromCircleToIndex(circleVector), neighbour ^ circleVector)
        })
      }
  }

  //@inline def compileTimeEvaluation(dimension: Int, radius: Int, noTransform: Boolean = false): Seq[Double] = ${evaluation(dimension, radius, noTransform)}

  def covTest(args: Array[String]): Unit = {

    def mean(seq: Seq[Double]): Double = {
      seq.sum / seq.size
    }

    def standardDeviation(seq: Seq[Double], seqMean: Double): Double = {
      sqrt(seq.map(_ - seqMean).map(pow(_, 2)).sum / seq.size)
    }

    def coefficientOfVariation(seq: Seq[Double]): Double = {
      val seqMean = mean(seq)
      standardDeviation(seq, seqMean) / seqMean
    }

    val dimension = 3
    val radius = 64
    //println(coefficientOfVariation(evaluation(dimension, radius, noTransform = true)))
    //println(coefficientOfVariation(evaluation(dimension, radius)))
  }

  def singleEvaluation(maxDimension: Int): Seq[Seq[Double]] = {
    (3 to maxDimension).map(dimension => {
      println(dimension)
      val squareVector = Seq(1.0, 0.5) ++ Seq.fill(dimension - 2)(0.1)
      val maxMagnitudeComponent = MaxMagnitude(squareVector).fullSpaceComponent
      val circleVector = fromSquareToCircle(squareVector).getOrElse(squareVector)
      Seq(squareVector ^ maxMagnitudeComponent, circleVector ^ maxMagnitudeComponent)
    }).transpose
  }

  def evaluationDiff(dimension: Int, radius: Int): Iterable[Double] = {
    println("Evaluation")
    circleWithIndex(dimension, radius)
      .map { case(circleVector, indexVector) =>
        (circleVector ^ MaxMagnitude(circleVector).fullSpaceComponent) - (indexVector.vector ^ MaxMagnitude(indexVector).fullSpaceComponent)
      }
  }

}
