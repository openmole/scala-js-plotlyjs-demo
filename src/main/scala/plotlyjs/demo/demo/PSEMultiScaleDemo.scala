package plotlyjs.demo.demo

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.openmole.plotlyjs.PlotlyImplicits._
import org.openmole.plotlyjs.ShapeType.rect
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import org.scalajs.dom.html
import plotlyjs.demo.utils.Colors._
import plotlyjs.demo.utils.Utils.{ExtraTraceManager, onDemand, printCode}
import plotlyjs.demo.utils.vector.IntVectors
import plotlyjs.demo.utils.vector.IntVectors._
import plotlyjs.demo.utils.vector.Vectors._
import plotlyjs.demo.utils.{Basis, PointSet}
import scaladget.bootstrapnative.bsn.btn_success

import scala.collection.immutable.HashMap
import scala.math._
import scala.scalajs.js
import scala.scalajs.js.JSConverters.JSRichIterableOnce

object PSEMultiScaleDemo {

  private lazy val sc = sourcecode.Text {

    def normalDistribution(mu: Double, sigma: Double): Double = {
      sqrt(-2 * log(random)) * cos(2 * Pi * random) * sigma + mu
    }

    case class MultiScaleBasis(sourceDimension: Int, subdivision: Int, destinationDimension: Int, allowStretch: Boolean = false, gap: Int = 1) extends Basis {

      val remainder: Int = sourceDimension % destinationDimension
      val stretchable: Boolean = remainder != 0
      val stretched: Boolean = allowStretch && stretchable

      private def _axisIndex(i: Int): Int = {
        i % destinationDimension
      }

      private def _scaleIndex(i: Int): Int = {
        i / destinationDimension
      }

      //Stretching
      private def axisIndex(i: Int): Int = {
        if(stretched && i >= destinationDimension) {
            _axisIndex(i + remainder)
        } else {
          _axisIndex(i)
        }
      }

      def scaleIndex(i: Int): Int = {
        if(stretched && axisIndex(i) < remainder) {
          _scaleIndex(i) + 1
        } else {
          _scaleIndex(i)
        }
      }
      //

      def axis(i: Int): Int = {
        axisIndex(i)
      }

      def scale(i: Int): Double = {
        pow(subdivision + gap, scaleIndex(i))
      }

      def destinationAxis(i: Int): Int = {
        axis(i) % destinationDimension
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

      val maxScaleIndex: Int = scaleIndex(sourceDimension - 1)

      def size(i: Int): Double = {
        (basisVector(sourceDimension - 1 - ((sourceDimension - 1 - i) % destinationDimension)) * subdivision).norm
      }

    }

    def subdivisionIndexOf(vector: Vector): IntVector = {
      toIntVector(vector.map(_.floor))
    }

    def analyse(subdivision: Int, patternSpaceMin: Vector, patternSpaceMax: Vector, points: Seq[Vector]) = {
      val dimension = patternSpaceMin.dimension;

      val boxes = IntVectors.positiveNCube(dimension, subdivision).toSeq.map(_.vector)
      val discovered = points.map(v => (v - patternSpaceMin)/(patternSpaceMax - patternSpaceMin)).map(_.scale(subdivision))
      val counts = {
        val countMap = discovered
          .groupBy(subdivisionIndexOf)
          .map { case (box, members) => (box, members.size) }
        boxes.map(countMap.getOrElse(_, 0))
      }
      val densities = {
        val maxCount = counts.max
        counts.map(_.toDouble / maxCount)
      }
      val boxCounts = HashMap.from(boxes.zip(counts));
      val boxDensities = HashMap.from(boxes.zip(densities))

      (boxes, discovered, boxCounts, boxDensities)
    }

    def sliceToString(basis: MultiScaleBasis, slice: IntVector): String = {
      slice.zipWithIndex.map { case (c, i) => s"o${i + basis.destinationDimension + 1} s$c" }.reduceOption(_ + ", " + _).getOrElse("")
    }

    def computePlotDiv(basis: MultiScaleBasis, discovered: Seq[IntVector], size: Int, parentContentVarOption: Option[Var[ReactiveHtmlElement[html.Div]]] = None, sliceOption: Option[IntVector] = None): ReactiveHtmlElement[html.Div] = {

      val discoveredShapeSeq = discovered
        .map(_.vector)
        .map { box =>
          val b0 = box
          val b1 = b0 + (0.0 at basis.sourceDimension).replace(0, 1.0).replace(1, 1.0)
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
              .width(0)
              .color(0.5 at 4)
            )
            .fillcolor(Seq(0.0, 1.0, 0.0))
            ._result
        }

      val (frameShapeSeq, hitboxDataSeq) = {
        val frameSeq = {
          val sliceSpaceDimension = basis.sourceDimension - basis.destinationDimension
          if(sliceSpaceDimension == 0) {
            Seq(Seq())
          } else {
            IntVectors.positiveNCube(sliceSpaceDimension, basis.subdivision).toSeq
          }
        }
        frameSeq
          .map((0.0 at basis.destinationDimension) ++ _.vector)
          .map { frame =>
            val s0 = frame
            val s1 = s0 + (0.0 at basis.sourceDimension).replace(0, basis.subdivision).replace(1, basis.subdivision)
            val points = Seq(s0, s1).map(basis.transform)
            val coordinates = points.transpose
            val (x0, y0) = (coordinates(0)(0), coordinates(1)(0))
            val (x1, y1) = (coordinates(0)(1), coordinates(1)(1))
            val frameShape = Shape
              .`type`(rect)
              .xref("x").yref("y")
              .x0(x0).x1(x1).y0(y0).y1(y1)
              .line(line
                .width(1)
                .color(0.5 at 4)
              )
              ._result
            val hitboxData = scatter
              .x(Seq((x0 + x1)/2.0).toJSArray)
              .y(Seq((y0 + y1)/2.0).toJSArray)
              .marker(marker
                .size(size/basis.subdivision.toDouble * 0.5)
                .symbol(square)
                .color(Seq(0.0, 1.0, 0.0))
                .opacity(0.0)
              )
              .hoverinfo("text")
              .text(sliceToString(basis, frame.drop(basis.destinationDimension)) + " – click to zoom")
              ._result
            (frameShape, hitboxData)
          }.unzip
      }

      lazy val boundsAnnotationSeq = {
        (0 until basis.destinationDimension)
          .reverse
          .map(basis.sourceDimension - 1 - _)
          .filter(i => basis.scaleIndex(i) == basis.maxScaleIndex)
          .flatMap(i => {
            (0 to basis.subdivision)
              .filter(s => if(size/basis.subdivision > 14) true else s % 2 == 0)
              .map(s => {
                val point = basis.transform(
                  (0.0 at basis.sourceDimension)
                    .replace(i, s)
                ).add({
                  val margin = basis.size(i)/size.toDouble * 32
                  val adjustmentFactor = basis.scaleIndex(i) match {
                    case 0 => 1
                    case 1 => 0.5
                    case _ => 1
                  }
                  (-(margin * adjustmentFactor) * (basis.scaleIndex(i) + 1) at basis.destinationDimension).replace(basis.axis(i), 0)
                })
                val text = s"o${i + 1} s$s"
                val textangle = basis.destinationAxis(i) match {
                  case 0 => -90
                  case 1 => 0
                  case _ => 0
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
      }

      def backupFunction(contentVar: Var[ReactiveHtmlElement[html.Div]]) = {
        val backup = contentVar.now()
        () => contentVar.set(backup)
      }

      val plotDiv = div()
      val plotDataSeq = if(basis.sourceDimension <= 2) Seq(scatter._result) else hitboxDataSeq
      Plotly.newPlot(
        plotDiv.ref,
        plotDataSeq.toJSArray,
        Layout
          .title("PSE" + (if(basis.stretched) " stretched" else "") + sliceOption.map(slice => " slice – " + sliceToString(basis, slice)).getOrElse(""))
          .width(size)
          .height(size)
          .margin(Margin
               .t(32 + 8)
            .l(0)     .r(0)
                 .b(0)
          )
          .showlegend(false)
          .xaxis(axis
            .visible(false)
          )
          .yaxis(axis
            .scaleanchor("x")
            .visible(false)
          )
          .shapes((discoveredShapeSeq ++ frameShapeSeq).toJSArray)
          .annotations(boundsAnnotationSeq.toJSArray)
          .hovermode("closest")
          ._result,
        Config
          .modeBarButtonsToRemove(Seq(
            "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
            "hoverClosestCartesian", "hoverCompareCartesian",
            "toggleSpikelines",
            //TODO remove plotly-logomark button
          ).toJSArray)
          .modeBarButtonsToAdd(js.Array(
            ModeBarButton
              .name("back")
              .icon(Icon // custom
                .width(2000)
                .height(1500)
                .path("m146.84375,14.552083 -6.61459,6.614583 6.61459,6.614584 0,-3.96875c 2.64583,0 9.26042,0 13.22917,5.291667 -3.96875,-10.583334 -10.58334,-10.583334 -13.22917,-10.583334z")
              )
              .click({
                val f = backupFunction(parentContentVarOption.getOrElse(Var(plotDiv)))
                _ => f()
              })
              ._result
          ))
      )

      if(parentContentVarOption.isEmpty) {
        val contentVar = Var(plotDiv)
        plotDiv.ref.on("plotly_click", pointsData => {
          val curveNumber = pointsData.points(0).curveNumber
          val slice = basis.sourceDimension match {
            case 3 =>
              Seq(curveNumber)
            case 4 =>
              val xSlice = curveNumber % basis.subdivision
              val ySlice = curveNumber / basis.subdivision
              Seq(xSlice, ySlice)
          }

          val sliceDiscovered = discovered
            .map(_.vector)
            .filter(_.takeRight(slice.vector.dimension).equals(slice))
            .map(_.take(basis.destinationDimension))
            .map(toIntVector)

          contentVar.set(computePlotDiv(basis.copy(sourceDimension = basis.destinationDimension), sliceDiscovered, size,
            Some(contentVar), Some(slice)))
        })
        div(child <-- contentVar.signal)
      } else {
        val onRelayout = backupFunction(parentContentVarOption.get)
        plotDiv.ref.on("plotly_relayout", onRelayout)
        plotDiv
      }
    }

    def maxSubdivisionBasis(dimension: Int, allowStretch: Boolean = false) = {
      MultiScaleBasis(dimension, ceil(pow(5000, 1d/dimension)).toInt, 2, allowStretch = allowStretch)
    }

    def maxSubdivisionPlotDiv(basis: MultiScaleBasis, size: Int): ReactiveHtmlElement[html.Div] = {
      val discovered = (0 to (0.25 * pow(basis.subdivision, basis.sourceDimension)).toInt).map(_ => (() => (random() * basis.subdivision).toInt.toDouble) at basis.sourceDimension).map(toIntVector)
      computePlotDiv(basis, discovered, size)
    }

    val size = 800

    div(
      onDemand("dimension = 2", _ => maxSubdivisionPlotDiv(maxSubdivisionBasis(2), size)),
      onDemand("dimension = 3", _ => maxSubdivisionPlotDiv(maxSubdivisionBasis(3), size)),
      //onDemand("dimension = 3, allowStretch = true", _ => comparisonDiv(maxSubdivisionBasis(3, allowStretch = true), size)),
      onDemand("dimension = 4", _ => maxSubdivisionPlotDiv(maxSubdivisionBasis(4), size)),
      onDemand("dimension = 6 (test de robustesse du code)", _ => maxSubdivisionPlotDiv(maxSubdivisionBasis(6), size)),
    )
  }

  val elementDemo: Demo = new Demo {
    def title: String = "PSE multi-scale"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
