package tools

object Vectors {

  def norm(p: Int, v: Seq[Double]): Double = math.pow(v.map(math.abs).map(math.pow(_, p)).sum, 1.0/p)
  def length(v: Seq[Double]): Double = norm(2, v)

  def scale(v: Seq[Double], s: Double): Seq[Double] = v.map(_ * s)
  def negate(v: Seq[Double]): Seq[Double] = scale(v, -1)

  def normalize(p: Int, v: Seq[Double]): Seq[Double] = {
    val vNorm = norm(p, v)
    if(vNorm != 0) scale(v, 1/vNorm) else v
  }
  def normalize(v: Seq[Double]): Seq[Double] = normalize(2, v)

  def toLength(v: Seq[Double], length: Double): Seq[Double] = scale(normalize(v), length)

  def add(v1: Seq[Double], v2: Seq[Double]): Seq[Double] = v1 zip v2 map { case (c1, c2) => c1 + c2 }
  def sub(v1: Seq[Double], v2: Seq[Double]): Seq[Double] = v1 zip v2 map { case (c1, c2) => c1 - c2 }
  def mul(v1: Seq[Double], v2: Seq[Double]): Seq[Double] = v1 zip v2 map { case (c1, c2) => c1 * c2 }

  def dotProduct(v1: Seq[Double], v2: Seq[Double]): Double = mul(v1, v2) reduce[Double] { case (c1, c2) => c1 + c2 }
  def angle(v1: Seq[Double], v2: Seq[Double]): Double = math.acos(dotProduct(v1, v2) / (length(v1) * length(v2)))

  /*
  def projection(v1: Seq[Double], v2: Seq[Double]): Seq[Double] = mul(v2, dotProduct(v1, v2))
  def orthogonal(v1: Seq[Double], v2: Seq[Double]): Seq[Double] = sub(v1, projection(v1, v2))
  */
}
