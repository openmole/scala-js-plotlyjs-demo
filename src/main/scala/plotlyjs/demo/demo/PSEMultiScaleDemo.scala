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

    def computePlotDiv(basis: MultiScaleBasis, discovered: Seq[IntVector], size: Int, contentOption: Option[Var[ReactiveHtmlElement[html.Div]]] = None, sliceOption: Option[IntVector] = None): ReactiveHtmlElement[html.Div] = {

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
                .opacity(0.0)
              )
              ._result
            (frameShape, hitboxData)
          }.unzip
      }

      lazy val boundsAnnotationSeq = {
        (0 until basis.sourceDimension).flatMap(i => {

          //Repeating 0-scaleIndex legend in 1-scaleIndex space.
          val shiftSeq = if(basis.scaleIndex(i) == 0 && basis.destinationDimension + i < basis.sourceDimension) {
            (1 until basis.subdivision).map(s => {
              (0.0 at basis.sourceDimension).replace(basis.destinationDimension + i, s)
            })
          } else Seq()
          ((0.0 at basis.sourceDimension) +: shiftSeq).flatMap(shift => {

            (0 to basis.subdivision).map(s => {
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
                    case _ => 0
                  }
                }) * 1.5
                (-margin * (basis.scaleIndex(i) + 1) at basis.destinationDimension).replace(basis.axis(i), 0)
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

        })
      }

      val plotDiv = div()
      val plotDataSeq = if(basis.sourceDimension <= 2) Seq(scatter._result) else hitboxDataSeq
      Plotly.newPlot(
        plotDiv.ref,
        plotDataSeq.toJSArray,
        Layout
          .title("PSE" + (if(basis.stretched) " stretched" else "" + sliceOption.map { slice =>
            " slice " + slice.zipWithIndex.map { case (c, i) => s"o${i + basis.destinationDimension + 1} s$c" }.reduce(_ + ", " + _)
          }.getOrElse("")))
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
          .shapes((discoveredShapeSeq ++ frameShapeSeq).toJSArray)
          .annotations(boundsAnnotationSeq.toJSArray)
          .hovermode("closest")
          ._result,
        Config
          .modeBarButtonsToRemove(Seq(
            "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
            "hoverClosestCartesian", "hoverCompareCartesian",
            "toggleSpikelines",
          ).toJSArray)
      )

      if(contentOption.isEmpty) {
        val content = Var(plotDiv)
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

          content.set(computePlotDiv(basis.copy(sourceDimension = basis.destinationDimension), sliceDiscovered, size,
            Some(content), Some(slice)))
        })
        div(child <-- content.signal)
      } else {
        val content = contentOption.get
        val backup = content.now()
        plotDiv.ref.on("plotly_relayout", () => {
          content.set(backup)
        })
        plotDiv
      }
    }

    /*
    def subplotDiv(msb: MultiScaleBasis, patternSpaceMin: Vector, patternSpaceMax: Vector, points: Seq[Vector], size: Int): ReactiveHtmlElement[html.Div] = {
      val subplotDimension = msb.sourceDimension - msb.destinationDimension
      val basis = new MultiScaleBasis(msb.sourceDimension, msb.subdivision, msb.destinationDimension, msb.allowStretch, msb.gap) {

        override def transform(vector: Vector): Vector = {
          val v = super.transform(vector.take(destinationDimension) ++ (0 at subplotDimension))
          if(stretched) {
            v.replace(0, _/(subdivision + gap)) // no longer stretched
          } else {
            v
          }
        }

      }

      val (boxes, discovered, boxCounts, boxDensities) = analyse(basis.subdivision, patternSpaceMin, patternSpaceMax, points);

      val zipped = (toIntVector(0 at subplotDimension) +: IntVectors.positiveNCube(subplotDimension, basis.subdivision).toSeq).distinct.map(subplotIndexVector => {
        val subplotIndex = (subplotDimension match {
          case 0 => 1
          case 1 => basis.subdivision - 1 - subplotIndexVector(0)
          case 2 => subplotIndexVector(0) + basis.subdivision * (basis.subdivision - 1 - subplotIndexVector(1))
        }) + 1

        def predicate(vector: Vector) = {
          subdivisionIndexOf(vector).drop(basis.destinationDimension) == subplotIndexVector
        }

        def filter(seq: Seq[Vector]) = {
          seq.filter(predicate)
        }

        val data = {
          val coordinates = filter(discovered).map(basis.transform).transpose
          Option.when(coordinates.nonEmpty)({
            scatter
              .x(coordinates(0).toJSArray)
              .y(coordinates(1).toJSArray)
              .xaxis("x" + subplotIndex)
              .yaxis("y" + subplotIndex)
              .marker(marker
                .size(2)
                .set(Seq(0.0, 0.0, 1.0).opacity(0.5))
              )
              ._result
          }).getOrElse(scatter._result)
        }

        lazy val shapeSeq = boxDensities
          .filter(bd => predicate(bd._1))
          .map { case (box, density) =>
            val b0 = box
            val b1 = b0 + (0.0 at basis.sourceDimension).replace(0, 1.0).replace(1, 1.0)
            val points = Seq(b0, b1).map(basis.transform)
            val coordinates = points.transpose
            Shape
              .`type`(rect)
              .xref("x" + subplotIndex)
              .yref("y" + subplotIndex)
              .x0(coordinates(0)(0))
              .x1(coordinates(0)(1))
              .y0(coordinates(1)(0))
              .y1(coordinates(1)(1))
              .line(line
                .width(1)//.width(0)
                .color(0.5 at 4)
              )
              .fillcolor(Seq(1.0, 0.0, 0.0).opacity(if(density == 0) 0 else 0.1 + density * 0.6))
              .layer("above")
              ._result
          }.toSeq

        (data, shapeSeq)
      })
      val (plotDataSeq, shapeSeqSeq) = zipped.unzip
      lazy val shapeSeq = shapeSeqSeq.flatten

      val boundsAnnotationSeq = {
        (basis.destinationDimension until basis.sourceDimension).flatMap(i => {

          (0 to basis.subdivision).map(s => {
            val point = (0.0 at basis.destinationDimension).replace(basis.axis(i), s)
              .add({
                val margin = 0.35
                (-margin at basis.destinationDimension).replace(basis.axis(i), 0)
              })
            val text = {
              s.toString
            }
            val textangle = basis.axis(i) match {
              case 0 => 0
              case 1 => -90
            }
            Annotation
              .x(point(0) / basis.subdivision).xref("paper")
              .y(point(1) / basis.subdivision).yref("paper")
              .xanchor("center")
              .yanchor("center")
              .text(text)
              .textangle(textangle)
              .showarrow(false)
              ._result
          })

        })
      }
      val layout = {

        var layout = Layout
          .title("PSE subplots" + (if(basis.stretched) " stretched" else ""))
          //.autosize(true)
          .width(size)
          .height({
            if(basis.sourceDimension == 3 && !basis.stretched) {
              size//printCode(size/basis.subdivision.toDouble)
            } else {
              size
            }
          })
          .showlegend(false)
          //.shapes(shapeSeq.toJSArray)
          .annotations(boundsAnnotationSeq.toJSArray)

        if(subplotDimension != 0)  {
          layout = layout
            .grid(grid
              .rows({
                basis.sourceDimension match {
                  case 3 => if(basis.stretched) basis.subdivision else 1
                  case 4 => basis.subdivision
                }
              })
              .columns({
                basis.sourceDimension match {
                  case 3 => if(basis.stretched) 1 else basis.subdivision
                  case 4 => basis.subdivision
                }
              })
              .pattern(Pattern.independent)
            )
        }

        (1 to plotDataSeq.size).map(_.toString).foreach(indexString => {
          layout = layout
            .asJsOpt("xaxis" + indexString, axis
              .range(0, basis.subdivision)
              .dtick(1)
            )
            .asJsOpt("yaxis" + indexString, {
              var yaxis = axis
                .range(0, basis.subdivision)
                //.fixedrange(true)
                .dtick(1)
              if(!basis.stretched) {
                yaxis = yaxis
                  .scaleanchor("x" + (if(indexString == "1") "" else indexString))
              }
              yaxis
            })
        })

        layout._result
      }

      val plotDiv = div()
      Plotly.newPlot(
        plotDiv.ref,
        plotDataSeq.toJSArray,
        layout
      )

      plotDiv
    }
    */

    def comparisonDiv(basis: MultiScaleBasis, size: Int) = {

      val patternSpaceMin = 0 at basis.sourceDimension
      val patternSpaceMax = 1 at basis.sourceDimension
      val points = (1 to 1024).map(_ => (() => normalDistribution(0.5, 0.1)) at basis.sourceDimension)

      val discovered = (0 to (0.25 * pow(basis.subdivision, basis.sourceDimension)).toInt).map(_ => (() => (random() * basis.subdivision).toInt.toDouble) at basis.sourceDimension).map(toIntVector)

      div(
        computePlotDiv(basis, discovered, size),
        //subplotDiv(basis, patternSpaceMin, patternSpaceMax, points, size)
      )
    }

    def maxSubdivisionBasis(dimension: Int, allowStretch: Boolean = false) = {
      MultiScaleBasis(dimension, ceil(pow(5000, 1d/dimension)).toInt, 2, allowStretch = allowStretch)
    }

    val size = 800

    div(
      onDemand("dimension = 2", _ => comparisonDiv(maxSubdivisionBasis(2), size)),
      onDemand("dimension = 3", _ => comparisonDiv(maxSubdivisionBasis(3), size)),
      //onDemand("dimension = 3, allowStretch = true", _ => comparisonDiv(maxSubdivisionBasis(3, allowStretch = true), size)),
      onDemand("dimension = 4", _ => comparisonDiv(maxSubdivisionBasis(4), size)),
      onDemand("dimension = 6 (test de robustesse du code)", _ => comparisonDiv(maxSubdivisionBasis(6), size)),
    )
  }

  val elementDemo: Demo = new Demo {
    def title: String = "PSE multi-scale"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
