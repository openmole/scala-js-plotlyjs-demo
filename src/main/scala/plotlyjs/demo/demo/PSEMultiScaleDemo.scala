package plotlyjs.demo.demo

import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.PlotMode.text
import org.openmole.plotlyjs.PlotlyImplicits._
import org.openmole.plotlyjs.ShapeType.rect
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import plotlyjs.demo.utils.vector.IntVectors._
import plotlyjs.demo.utils.{Basis, PointSet, Utils}
import plotlyjs.demo.utils.Colors._
import plotlyjs.demo.utils.vector.IntVectors
import plotlyjs.demo.utils.vector.Vectors._

import scala.math._
import scala.scalajs.js.JSConverters.JSRichIterableOnce

object PSEMultiScaleDemo { //TODO with subplots ?

  private lazy val sc = sourcecode.Text {

    def normalDistribution(mu: Double, sigma: Double): Double = {
      sqrt(-2 * log(random)) * cos(2 * Pi * random) * sigma + mu
    }

    case class MultiScaleBasis(sourceDimension: Int, subdivision: Int, destinationDimension: Int, stretchable: Boolean = false, gap: Int = 1) extends Basis {

      val remainder: Int = sourceDimension % destinationDimension
      val complement: Int = (destinationDimension - remainder) % destinationDimension
      val stretched: Boolean = stretchable && complement != 0

      private def _i(i: Int) = {
        if(stretched) complement + i else i
      }

      def scaleIndex(i: Int): Int = {
        _i(i) / destinationDimension
      }

      def scale(i: Int): Double = {
        pow(subdivision + gap, scaleIndex(i))
      }

      def axis(i: Int): Int = {
        _i(i) % destinationDimension
      }

      override val size: Int = sourceDimension

      override def basisVector(i: Int): Vector = {
        (0.0 at destinationDimension).replace(axis(i), 1.0) * scale(i)
      }

      override def transform(vector: Vector): Vector = {
        if((destinationDimension until sourceDimension).map(vector(_).isWhole).reduceOption(_ && _).getOrElse(true)) {
          super.transform(vector)
        } else {
          throw new IllegalArgumentException(s"Coordinates from index $destinationDimension until $sourceDimension must be whole.")
        }
      }

    }

    val dimension = 4
    val subdivision = 5

    val basis = MultiScaleBasis(dimension, subdivision, 2, stretchable = true)

    val boxes = IntVectors.positiveNCube(dimension, subdivision).toSeq.map(_.vector)

    val pointSet = new PointSet(
      Utils.randomizeDimensions((1 to 1024).map(_ => (() => normalDistribution(0.5, 0.125)) at dimension))
    )
    val discovered = pointSet.spaceNormalizedOutputs.map(_.scale(subdivision)) //TODO use bounds to be specified for each dimension.
    //val discovered = new PointSet(Data.pse.map(_.values).transpose).spaceNormalizedOutputs.map(scale(subdivision))

    val counts = {
      val countMap = discovered
        .groupBy(_.map(_.floor))
        .map { case (box, members) => (box, members.size) }
      boxes.map(countMap.getOrElse(_, 0))
    }
    val densities = {
      val maxCount = counts.max
      counts.map(_.toDouble / maxCount)
    }

    val boxesShapeSeq = boxes.zip(densities).map { case (box, density) =>
      val b0 = box
      val b1 = b0 + (0.0 at dimension).replace(0, 1.0).replace(1, 1.0)
      val points = Seq(b0, b1).map(basis.transform)
      val coordinates = points.transpose
      Shape
        .`type`(rect)
        .xref("x")
        .yref("y")
        .x0(coordinates(0)(0))
        .x1(coordinates(0)(1))
        .y0(coordinates(1)(0))
        .y1(coordinates(1)(1))
        .line(line
          .width(1)
          .color(0.5 at 4)
        )
        .fillcolor(Seq(1.0, 0.0, 0.0).opacity(if(density == 0) 0 else 0.1 + density * 0.6))
        .layer("above")
        ._result
    }

    val boundsAnnotationSeq = {
      (0 until basis.sourceDimension).flatMap(i => {

        //Repeating 0-scaleIndex legend in 1-scaleIndex space.
        val shiftSeq = if(basis.scaleIndex(i) == 0 && basis.destinationDimension + i < basis.sourceDimension) {
          (1 until subdivision).map(s => {
            (0.0 at basis.sourceDimension).replace(basis.destinationDimension + i, s)
          })
        } else Seq()
        ((0.0 at basis.sourceDimension) +: shiftSeq).flatMap(shift => {

          (0 to subdivision).map(s => {
            val point = basis.transform(
              (0.0 at basis.sourceDimension)
                .replace(i, s)
                .add(shift)
            ).add({
              val margin = (basis.sourceDimension match {
                case 2 => 0.25
                case _ => basis.scaleIndex(i) match {
                  case 0 => 1.5
                  case 1 => 2
                }
              }) * 1.5
              (-margin * (basis.scaleIndex(i) + 1) at basis.destinationDimension).replace(basis.axis(i), 0)
            })
            val text = {
              val values = pointSet.rawOutputs.map(_(i))
              val minValue = values.min
              val maxValue = values.max
              val bound = minValue + (maxValue - minValue) * s/subdivision
              s"o${i + 1} = %.2f".format(bound)
            }
            val textangle = basis.axis(i) match {
              case 0 => -90
              case 1 => 0
            }
            Annotation
              .x(point(0))
              .y(point(1))
              .text(text)
              .textangle(textangle)
              .showarrow(false)
              ._result
          })

        })

      })
    }
    val data = {
      val coordinates = discovered
        .map(_.zipWithIndex.map { case (c, i) => if(i < 2) c else floor(c) })
        .map(basis.transform)
        .transpose
      scatter
        .x(coordinates(0).toJSArray)
        .y(coordinates(1).toJSArray)
        .marker(marker
          .size(2)
          .set(Seq(0.0, 0.0, 1.0).opacity(0.5))
        )
        ._result
    }

    val plotDiv = div()
    val plotDataSeq = Seq(data)
    val size = 800
    Plotly.newPlot(
      plotDiv.ref,
      plotDataSeq.toJSArray,
      Layout
        .title("PSE" + (if(basis.stretched) " stretched" else ""))
        .width(size)
        .height(size)
        .showlegend(false)
        .xaxis(axis
          .visible(false)
        )
        .yaxis(axis
          .scaleanchor("x")
          .visible(false)
        )
        .shapes(boxesShapeSeq.toJSArray)
        .annotations(boundsAnnotationSeq.toJSArray)
        ._result
    )

    plotDiv
  }

  val elementDemo: Demo = new Demo {
    def title: String = "PSE multi-scale"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
