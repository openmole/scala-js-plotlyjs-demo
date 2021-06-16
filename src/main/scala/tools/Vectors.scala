package tools

object Vectors {

  def norm(p: Int, v: Seq[Double]): Double = math.pow(v.map(math.abs).map(math.pow(_, p)).sum, 1.0/p)
  def mul(v: Seq[Double], s: Double): Seq[Double] = v.map(_ * s)
  def normalize(p: Int, v: Seq[Double]): Seq[Double] = mul(v, 1/norm(p, v))

  def add(v1: Seq[Double], v2: Seq[Double]): Seq[Double] = v1 zip v2 map { case (c1, c2) => c1 + c2 }
  def sub(v1: Seq[Double], v2: Seq[Double]): Seq[Double] = v1 zip v2 map { case (c1, c2) => c1 - c2 }
  def mul(v1: Seq[Double], v2: Seq[Double]): Seq[Double] = v1 zip v2 map { case (c1, c2) => c1 * c2 }

  def dotProduct(v1: Seq[Double], v2: Seq[Double]): Double = mul(v1, v2) reduce[Double] { case (c1, c2) => c1 + c2 }
  def projection(v1: Seq[Double], v2: Seq[Double]): Seq[Double] = mul(v2, dotProduct(v1, v2))
  def orthogonal(v1: Seq[Double], v2: Seq[Double]): Seq[Double] = sub(v1, projection(v1, v2))

}
