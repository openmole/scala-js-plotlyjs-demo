package plotlyjs.demo.directions

import scala.math._
import plotlyjs.demo.utils.Vectors._

object NSphereCovering {

  def nSphereCovering(n: Int, alphaStep: Double): Seq[Seq[Double]] = {
    val dimension = n + 1
    if(n == 0) {
      Seq(Seq(-1.0), Seq(1.0))
    } else {
      val alphaMax = acos(1/sqrt(dimension)) // angle((1,...,1), (1,0,...,0))
      (1 to (alphaMax/alphaStep).toInt).foreach(i => { //change to map
        val rOnCell = tan(i * alphaStep)
        val rOnSphere = Seq(rOnCell, 1).normalize.head
        val vectors = nSphereCovering(n - 1, alphaStep/rOnSphere)
          .map(scale(rOnSphere))
          //.flatMap(_.inse)
          .map(normalize)
      })

      Seq()
    }
  }

}
