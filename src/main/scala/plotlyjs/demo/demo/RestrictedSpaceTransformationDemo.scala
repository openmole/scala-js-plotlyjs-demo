package plotlyjs.demo.demo

import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.PlotMode._
import org.openmole.plotlyjs.PlotlyImplicits._
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import plotlyjs.demo.directions.{RestrictedSpaceTransformation, RestrictedSpaceTransformation2, RestrictedSpaceTransformation3, RestrictedSpaceTransformation4}
import plotlyjs.demo.utils.Data
import plotlyjs.demo.utils.Utils.onDemand
import plotlyjs.demo.utils.Vectors._

import scala.scalajs.js.JSConverters.JSRichIterableOnce

object RestrictedSpaceTransformationDemo {

  private val sc = sourcecode.Text {

    def lineChartDiv(dimension: Int) = {
      val plotDiv = div()

      import plotlyjs.demo.directions.RestrictedSpaceTransformation4.F
      val f = F(dimension, null, 1)
      val plotDataSeq = {
        Seq[(String, Double => Double)](
          ("regularization", f.regularization),
          ("projection", f.projection),
          ("adjustmentFactor", f.adjustmentFactor),
          ("adjustmentProportion", f.adjustmentProportion),
          ("inverseRegularizationTest", r => f.inverseRegularizationTest(r)),
        )
        .map { case (name, function) =>
          val n = 100
          val xs = (0.0000001 +: (1 to n).map(_.toDouble)).map(_ / n)
          val ys = xs.map(function(_))
          linechart.lines
            .name(name)
            .x(xs.toJSArray)
            .y(ys.toJSArray)
            .setMode(lines)
            ._result
        }
      }

      Plotly.plot(plotDiv.ref, plotDataSeq.toJSArray)

      plotDiv
    }

    def onDemandLineChartDiv(dimension: Int) = {
      onDemand(s"Functions graph – dimension $dimension", () => lineChartDiv(dimension))
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
    lazy val sphere = RestrictedSpaceTransformation.fromSquareToCircle(cube)

    def fromSquareToCircle(n: Int, points: Seq[Vector]) = {
      var seq = Seq(points)
      for(_ <- 0 until n) seq = seq :+ RestrictedSpaceTransformation.fromSquareToCircle(seq.reverse.head)
      seq
    }

    def fromCircleToSquare(n: Int, points: Seq[Vector]) = {
      var seq = Seq(points)
      for(_ <- 0 until n) seq = seq :+ seq.reverse.head.map(RestrictedSpaceTransformation.fromCircleToSquare)
      seq
    }

    div(
      onDemandLineChartDiv(3),
      onDemandLineChartDiv(30),
      onDemand("RST4", () => scatter3dDiv("RST4", RestrictedSpaceTransformation4.fromSquareToCircle(Data.centeredNCube(3, 32, hollow = true)))),
      onDemand("RST4 inv", () => scatter3dDiv("RST4 inv", RestrictedSpaceTransformation4.fromSquareToCircle(Data.centeredNCube(3, 64, hollow = true)).map(RestrictedSpaceTransformation4.fromCircleToSquare))),
      onDemand("Load", () => div(fromSquareToCircle(3, cube).zipWithIndex.map { case(points, i) => scatter3dDiv(s"From square to circle – $i times", points) } ++ fromCircleToSquare(3, sphere).zipWithIndex.map { case(points, i) => scatter3dDiv(s"From circle to square – $i times", points) }))
    )
  }

  val elementDemo: ElementDemo = new ElementDemo {
    def title: String = "Restricted space transformation"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

  def mainTest(args: Array[String]): Unit = {
    println(RegularDirectionsDemo.elementDemo)
  }

}
