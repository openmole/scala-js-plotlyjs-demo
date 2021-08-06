package plotlyjs.demo.utils

import plotlyjs.demo.utils.vector.Vectors._

trait Basis {

  val size: Int

  def basisVector(i: Int): Vector

  def component(vector: Vector, i: Int): Vector = {
    vector(i) * basisVector(i)
  }

  def transform(vector: Vector): Vector = {
    (0 until size).map(component(vector, _)).reduce(_ + _)
  }

}
