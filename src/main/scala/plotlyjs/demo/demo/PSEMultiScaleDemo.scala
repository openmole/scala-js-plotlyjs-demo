package plotlyjs.demo.demo

import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.PlotMode._
import org.openmole.plotlyjs.PlotlyImplicits._
import org.openmole.plotlyjs.ShapeType.rect
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import plotlyjs.demo.directions.restrictedspacetransformation.v4.IndexVectors
import plotlyjs.demo.directions.restrictedspacetransformation.v4.IndexVectors._
import plotlyjs.demo.utils.{Basis, Data, PointSet}
import plotlyjs.demo.utils.Colors._
import plotlyjs.demo.utils.Vectors._

import scala.math._
import scala.scalajs.js.JSConverters.JSRichIterableOnce

object PSEMultiScaleDemo {

  private lazy val sc = sourcecode.Text {

    def normalDistribution(mu: Double, sigma: Double): Double = {
      sqrt(-2 * log(random)) * cos(2 * Pi * random) * sigma + mu
    }

    class MultiScaleBasis(sourceDimension: Int, subdivision: Int, gap: Double, destinationDimension: Int, stretchable: Boolean = false) extends Basis {

      val remainder: Int = sourceDimension % destinationDimension
      val complement: Int = (destinationDimension - remainder) % destinationDimension
      val stretched: Boolean = stretchable && complement != 0

      override def basisVector(i: Int): Vector = {
        val _i = if(stretched) complement + i else i
        val scale = pow(subdivision * (1 + gap), _i / destinationDimension)
        val axis = _i % destinationDimension
        (0.0 at destinationDimension).replace(axis, 1.0) * scale
      }

    }

    /*
    def multiScaleBasis(sourceDimension: Int, subdivision: Int, gap: Double, destinationDimension: Int) = {

      val dimensionExtension = 0//destinationDimension - (sourceDimension % destinationDimension)
      val extendedDimension = dimensionExtension + sourceDimension

      new MultiScaleBasis(subdivision, gap, extendedDimension) {

        override def component(vector: Vector, i: Int): Vector = {
          super.component((subdivision / 2.0 at dimensionExtension) ++ vector, i)
        }

      }
    }
    */

    val dimension = 6
    val subdivision = 5
    val gap = 1.0/4

    val basis = new MultiScaleBasis(dimension, subdivision, gap, 2, true)

    val boxes = IndexVectors.positiveNCube(dimension, subdivision).toSeq.map(toVector)
    val discovered = (1 to 1024).map(_ => (() => normalDistribution(0.5, 0.1) * subdivision) at dimension)
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

    /*
    val boxesDataSeq = boxes.map(box => {
      val b00 = box
      val b10 = b00 + (0.0 at dimension).replace(0, 1.0)
      val b01 = b00 + (0.0 at dimension).replace(1, 1.0)
      val b11 = b00 + (0.0 at dimension).replace(0, 1.0).replace(1, 1.0)
      val points = {
        val edge0 = (box(0) == subdivision - 1)
        val edge1 = (box(1) == subdivision - 1)
        if(edge0 && edge1) {
          Seq(b10, b00, b01, b11, b10)
        } else if(edge0) {
          Seq(b11, b10, b00, b01)
        } else if(edge1) {
          Seq(b10, b00, b01, b11)
        } else {
          Seq(b10, b00, b01)
        }
      }.map(basis.transform)
      val coordinates = points.transpose
      scatter
        .x(coordinates(0).toJSArray)
        .y(coordinates(1).toJSArray)
        .setMode(lines)
        .line(line
          .color(0.5 at 3)
        )
        //.marker(marker.size(4))
        .hoverinfo("none")
        ._result

    })
    */

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
        .fillcolor(Seq(1.0, 0.0, 0.0).withAlpha(density * 0.6))
        .layer("above")
        ._result
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
          .set(Seq(0.0, 0.0, 1.0).withAlpha(0.5))
        )
        ._result
    }

    val plotDiv = div()
    val plotDataSeq = /*boxesDataSeq ++*/ Seq(data)
    val size = 800
    Plotly.newPlot(
      plotDiv.ref,
      plotDataSeq.toJSArray,
      Layout
        .shapes(boxesShapeSeq.toJSArray)
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
