package plotlyjs.demo.demo

import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.PlotMode._
import org.openmole.plotlyjs.PlotlyImplicits._
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import plotlyjs.demo.directions.restrictedspacetransformation.v4._
import plotlyjs.demo.utils.Data
import plotlyjs.demo.utils.Utils.onDemand
import plotlyjs.demo.utils.Vectors._

import scala.scalajs.js.JSConverters.JSRichIterableOnce

object RestrictedSpaceTransformationDemo {

  private lazy val sc = sourcecode.Text {

    def lineChartDiv(dimension: Int) = {
      val plotDiv = div()

      val g = new Geometry(dimension, null, 1)
      val plotDataSeq = {
        Seq[(String, Double => Double)](
          ("regularization", g.regularization),
          ("projection", g.projection),
          ("adjustmentFactor", g.adjustmentFactor),
          ("adjustmentProportion", g.adjustmentProportion),
          ("adjustment", r => g.regularization(r) * g.adjustmentProportion(r)),
          //("inverseRegularizationTest", r => g.inverseRegularizationTest(r)),
        )
        .map { case (name, function) =>
          val n = 100
          val xs = (0 to n).map(_.toDouble).map(_ / n)
          val ys = xs.map(function(_))
          linechart.lines
            .name(name)
            .x(xs.toJSArray)
            .y(ys.toJSArray)
            .setMode(lines)
            ._result
        }
      }

      val layout = Layout.title(s"Functions graph – dimension $dimension")

      Plotly.plot(plotDiv.ref, plotDataSeq.toJSArray, layout)

      plotDiv
    }

    def scatter3dData(points: Seq[Seq[Double]]) = {
      val pointsT = points.transpose
      scatter3d
        .x(pointsT(0).toJSArray)
        .y(pointsT(1).toJSArray)
        .z(pointsT(2).toJSArray)
        .setMode(markers)
        .marker(marker
          .size(1)
          .symbol(circle)
          .color(Color.rgb(0, 123, 255))
        )._result
    }

    def scatter3dDiv(title: String, points: Seq[Seq[Double]]) = {
      val plotDiv = div()

      val plotDataSeq = Seq(scatter3dData(points))

      val layout = Layout.title(title)

      Plotly.plot(plotDiv.ref, plotDataSeq.toJSArray, layout)

      plotDiv
    }

    val dimension = 3
    val p = 31
    lazy val cube = Data.centeredNCube(dimension, p, hollow = true)
    lazy val cubeSectors0 = cube.filter(v => MaxMagnitude(MaxMagnitude(v).remainderSpaceRemainder).index == 0)
    lazy val sphere = cube.flatMap(Transformation.fromSquareToCircle)

    def fromSquareToCircle(n: Int, points: Seq[Vector]) = {
      var seq = Seq(points)
      for(_ <- 0 until n) seq = seq :+ seq.reverse.head.flatMap(Transformation.fromSquareToCircle)
      seq
    }

    def fromCircleToSquare(n: Int, points: Seq[Vector]) = {
      var seq = Seq(points)
      for(_ <- 0 until n) seq = seq :+ seq.reverse.head.map(Transformation.fromCircleToSquare)
      seq
    }

    div(
      lineChartDiv(3),
      lineChartDiv(30),
      lineChartDiv(300),
      onDemand("Load", _ => div(fromSquareToCircle(3, cube).zipWithIndex.map { case(points, i) => scatter3dDiv(s"From square to circle – $i times", points) } ++ fromCircleToSquare(3, sphere).zipWithIndex.map { case(points, i) => scatter3dDiv(s"From circle to square – $i times", points) }))
    )
  }

  val elementDemo: Demo = new Demo {
    def title: String = "Restricted space transformation"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

  def mainTest(args: Array[String]): Unit = {
    println(RegularDirectionsDemo.elementDemo)
  }

}
