package plotlyjs.demo.homemade.utils

import plotlyjs.demo.homemade.utils.Vectors._

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
