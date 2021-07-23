package plotlyjs.demo.utils

import plotlyjs.demo.utils.Vectors._

trait Basis {

  def basisVector(i: Int): Vector

  def component(vector: Vector, i: Int): Vector = {
    vector(i) * basisVector(i)
  }

  def transform(vector: Vector): Vector = {
    (0 until vector.dimension).map(component(vector, _)).reduce(_ + _)
  }

}
