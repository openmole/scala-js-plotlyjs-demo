package plotlyjs.demo.labo

import com.raquo.airstream.state.Var
import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.openmole.plotlyjs.PlotlyImplicits.elToPlotlyElement
import org.openmole.plotlyjs.ShapeType.rect
import org.openmole.plotlyjs.all.{axis, line, marker, scatter, square}
import org.openmole.plotlyjs._
import org.scalajs.dom.html
import plotlyjs.demo.demo.Demo
import plotlyjs.demo.utils.Colors._
import plotlyjs.demo.utils.Utils.onDemand
import plotlyjs.demo.utils.vector.IntVectors
import plotlyjs.demo.utils.vector.IntVectors._
import plotlyjs.demo.utils.vector.Vectors._
import plotlyjs.demo.utils.{Basis, Data, PointSet}

import scala.collection.immutable.HashMap
import scala.math._
import scala.scalajs.js
import scala.scalajs.js.JSConverters.JSRichIterableOnce

object PSEMultiScaleDemo {

  private lazy val sc = sourcecode.Text {

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
        if (stretched && i >= destinationDimension) {
          _axisIndex(i + remainder)
        } else {
          _axisIndex(i)
        }
      }

      def scaleIndex(i: Int): Int = {
        if (stretched && axisIndex(i) < remainder) {
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

      val maxScaleIndex: Int = scaleIndex(sourceDimension - 1)

      override val size: Int = sourceDimension

      override def basisVector(i: Int): Vector = {
        (0.0 at destinationDimension).replace(axis(i), 1.0) * scale(i)
      }

      override def transform(vector: Vector): Vector = {
        if ((destinationDimension until sourceDimension).map(vector(_).isWhole).reduceOption(_ && _).getOrElse(true)) {
          super.transform(vector)
        } else {
          throw new IllegalArgumentException(s"Coordinates from index $destinationDimension until $sourceDimension must be whole.")
        }
      }

      def totalSize(i: Int): Double = {
        val maxParallel = sourceDimension - 1 - ((sourceDimension - 1 - i) % destinationDimension)
        (basisVector(maxParallel) * subdivision).norm
      }

      def size(destinationAxis: Int): Double = {
        for (i <- 0 until sourceDimension) {
          if (axis(i) == destinationAxis) {
            return totalSize(i)
          }
        }
        -1
      }

    }

    def subdivisionIndexOf(vector: Vector): IntVector = {
      toIntVector(vector.map(_.floor))
    }

    def analyse(subdivision: Int, patternSpaceMin: Vector, patternSpaceMax: Vector, points: Seq[Vector]) = {
      val dimension = patternSpaceMin.dimension;

      val boxes = IntVectors.positiveNCube(dimension, subdivision).toSeq.map(_.vector)
      val discovered = points.map(v => (v - patternSpaceMin) / (patternSpaceMax - patternSpaceMin)).map(_.scale(subdivision))
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

    def computePlotDiv(basis: MultiScaleBasis, discovered: Seq[IntVector], size: Int, replications: Seq[Int] = Seq(), parentContentVarOption: Option[Var[ReactiveHtmlElement[html.Div]]] = None, sliceOption: Option[IntVector] = None): ReactiveHtmlElement[html.Div] = {

      val _replications = if (replications.isEmpty) discovered.map(_ => 100) else replications

      val discoveredShapeSeq = {
        discovered
          .map(_.vector)
          .zip(_replications)
          .map { case (box, repli) =>
            val b0 = box
            val b1 = b0 + (0.0 at basis.sourceDimension).replace(0, 1.0).replace(1, 1.0)
            val points = Seq(b0, b1).map(basis.transform)
            val coordinates = points.transpose
            val replicability = repli / 100.0
            Shape
              .`type`(rect)
              .x0(coordinates(0)(0))
              .x1(coordinates(0)(1))
              .y0(coordinates(1)(0))
              .y1(coordinates(1)(1))
              .line(line
                .width(0)
                .color(0.5 at 4)
              )
              //.fillcolor((1 - replicability) * Seq(0.0, 0.0, 1.0) + replicability * Seq(1.0, 0.0, 0.0))
              .fillcolor(Seq(1.0, 0.0, 0.0).opacity(0.2 + replicability / 2.0))
              ._result
          }
      }

      val (frameShapeSeqSeq, hitboxDataSeq) = {
        val frameSeq = {
          val sliceSpaceDimension = basis.sourceDimension - basis.destinationDimension
          if (sliceSpaceDimension == 0) {
            Seq(Seq())
          } else {
            IntVectors.positiveNCube(sliceSpaceDimension, basis.subdivision).toSeq
          }
        }
        val customLine = line
          .width(1)
          .color(0.5 at 4)
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
              .x0(x0).x1(x1).y0(y0).y1(y1)
              .line(customLine)
              ._result
            val gridShapeSeq = if (basis.sourceDimension != 2) Seq() else {
              val lowSBound = 0
              val highSBound = basis.subdivision
              (lowSBound + 1 until highSBound)
                .flatMap(s => {
                  val p0H = basis.transform(frame.replace(0, lowSBound).replace(1, s))
                  val p1H = basis.transform(frame.replace(0, highSBound).replace(1, s))
                  val p0V = basis.transform(frame.replace(0, s).replace(1, lowSBound))
                  val p1V = basis.transform(frame.replace(0, s).replace(1, highSBound))
                  Seq(
                    Shape
                      .`type`("line")
                      .x0(p0H(0))
                      .y0(p0H(1))
                      .x1(p1H(0))
                      .y1(p1H(1))
                      .line(customLine)
                      ._result,
                    Shape
                      .`type`("line")
                      .x0(p0V(0))
                      .y0(p0V(1))
                      .x1(p1V(0))
                      .y1(p1V(1))
                      .line(customLine)
                      ._result
                  )
                })
            }
            val hitboxData = scatter
              .x(Seq((x0 + x1) / 2.0).toJSArray)
              .y(Seq((y0 + y1) / 2.0).toJSArray)
              .set(marker //.marker(
                .size(size / basis.subdivision.toDouble * 0.5)
                .set(square) //.symbol(
                .set(1 at 3) //.color(
                .opacity(0.0)
              )
              .hoverinfo("text")
              .text(sliceToString(basis, frame.drop(basis.destinationDimension)) + " – click to zoom")
              ._result
            (frameShape +: gridShapeSeq, hitboxData)
          }.unzip
      }
      val frameShapeSeq = frameShapeSeqSeq.flatten

      lazy val boundsAnnotationSeq = {
        val subdivisionSize = size / basis.subdivision.toDouble
        val annotationMinimumSize = 16 + 8
        val step = ceil(annotationMinimumSize / subdivisionSize).toInt
        (0 until basis.destinationDimension)
          .map(basis.sourceDimension - 1 - _).reverse
          .filter(i => basis.scaleIndex(i) == basis.maxScaleIndex)
          .flatMap(i => {
            val destinationAxis = basis.axis(i)
            val destinationScaleIndex = basis.scaleIndex(i);
            (0 until basis.subdivision)
              .filter(s => s % step == 0 /* || s == basis.subdivision*/)
              .map(s => {
                val pixelToPlot = basis.totalSize(i) / size.toDouble

                val point = ((basis.transform((0.0 at basis.sourceDimension).replace(i, s)) + {
                  if (basis.sourceDimension <= 2) {
                    basis.transform((0.0 at basis.sourceDimension).replace(i, s + 1))
                  } else {
                    basis.transform((0.0 at basis.sourceDimension).replace(i, s).replace(i - basis.destinationDimension, basis.subdivision))
                  }
                }) / 2)
                  .add({
                    val margin = 32 * pixelToPlot
                    val adjustmentFactor = destinationScaleIndex match {
                      case 0 => 1
                      case 1 => 0.5
                      case _ => 1
                    }
                    (-(margin * adjustmentFactor) * (destinationScaleIndex + 1) at basis.destinationDimension).replace(destinationAxis, 0)
                  })
                val text = "s" + s
                Annotation
                  .x(point(0))
                  .y(point(1))
                  .text(text)
                  .showarrow(false)
                  ._result
              }) :+ {
              destinationAxis match {
                case 0 => Annotation
                  .xref("paper").yref("paper")
                  .x(1).xanchor("right")
                  .y(0).yanchor("bottom")
                  .text("o" + (i + 1))
                  .showarrow(false)
                  ._result
                case 1 => Annotation
                  .xref("paper").yref("paper")
                  .x(0).xanchor("left")
                  .y(1).yanchor("top")
                  .text("o" + (i + 1))
                  .showarrow(false)
                  ._result
                case _ => Annotation.showarrow(false)._result
              }
            }
          })
      }

      def backupFunction(contentVar: Var[ReactiveHtmlElement[html.Div]]) = {
        val backup = contentVar.now()
        () => contentVar.set(backup)
      }

      val plotDiv = div()
      val plotDataSeq = if (basis.sourceDimension <= 2) Seq(scatter._result) else hitboxDataSeq
      val layout = {
        val subdivisionToPixel = size / max(basis.size(0), basis.size(1))
        val topMargin = 32 + 8
        val internalBottomMargin = 64
        Layout
          .title("PSE" + (if (basis.stretched) " stretched" else "") + sliceOption.map(slice => " slice – " + sliceToString(basis, slice)).getOrElse(""))
          .width(basis.size(0) * subdivisionToPixel)
          .height(basis.size(1) * subdivisionToPixel + topMargin + internalBottomMargin)
          .margin(Margin
            .t(topMargin)
            .l(0).r(0)
            .b(0)
          )
          .showlegend(false)
          .xaxis(axis
            .visible(false)
            //.dtick(1)
            //.showgrid(basis.sourceDimension == 2)
          )
          .yaxis(axis
            .scaleanchor("x")
            .visible(false)
            //.dtick(1)
            //.showgrid(basis.sourceDimension == 2)
          )
          .shapes((discoveredShapeSeq ++ frameShapeSeq).toJSArray)
          .annotations(boundsAnnotationSeq.toJSArray)
          .hovermode("closest")
          ._result
      }
      val config = {
        var config = Config
          .modeBarButtonsToRemove(Seq(
            "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
            "hoverClosestCartesian", "hoverCompareCartesian",
            "toggleSpikelines",
          ).toJSArray)
          .displaylogo(false)
        if (parentContentVarOption.isDefined) {
          config = config
            .modeBarButtonsToAdd(js.Array(
              ModeBarButton
                .name("Go back to overview")
                .icon(Icon // plotly home icon
                  .width(928.6)
                  .height(1000)
                  .path("m786 296v-267q0-15-11-26t-25-10h-214v214h-143v-214h-214q-15 0-25 10t-11 26v267q0 1 0 2t0 2l321 264 321-264q1-1 1-4z m124 39l-34-41q-5-5-12-6h-2q-7 0-12 3l-386 322-386-322q-7-4-13-4-7 2-12 7l-35 41q-4 5-3 13t6 12l401 334q18 15 42 15t43-15l136-114v109q0 8 5 13t13 5h107q8 0 13-5t5-13v-227l122-102q5-5 6-12t-4-13z")
                  .transform("matrix(1 0 0 -1 0 850)")
                )
                .click(backupFunction(parentContentVarOption.get))
                ._result
            ))
        }
        config
      }
      Plotly.newPlot(plotDiv.ref, plotDataSeq.toJSArray, layout, config._result)

      if (parentContentVarOption.isEmpty) {
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

          val (filteredDiscovered, filteredReplications) = discovered
            .map(_.vector)
            .zip(_replications)
            .filter(_._1.takeRight(slice.vector.dimension).equals(slice))
            .unzip
          val sliceDiscovered = filteredDiscovered
            .map(_.take(basis.destinationDimension))
            .map(toIntVector)

          contentVar.set(computePlotDiv(
            basis.copy(sourceDimension = basis.destinationDimension), sliceDiscovered, size, filteredReplications,
            Some(contentVar), Some(slice)
          ))
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
      val replications = discovered.indices.map(_ => (random() * 100).toInt)
      computePlotDiv(basis, discovered, size, replications)
    }

    val size = 800

    div(
      onDemand("dimension = 2", _ => maxSubdivisionPlotDiv(maxSubdivisionBasis(2), size)),
      onDemand("dimension = 3", _ => maxSubdivisionPlotDiv(maxSubdivisionBasis(3), size)),
      //onDemand("dimension = 3, allowStretch = true", _ => comparisonDiv(maxSubdivisionBasis(3, allowStretch = true), size)),
      onDemand("dimension = 4", _ => maxSubdivisionPlotDiv(maxSubdivisionBasis(4), size)),
      onDemand("dimension = 6 (test de robustesse du code)", _ => maxSubdivisionPlotDiv(maxSubdivisionBasis(6), size)),
      onDemand("adaptive legend", _ => div(maxSubdivisionPlotDiv(maxSubdivisionBasis(2), 800),
        maxSubdivisionPlotDiv(maxSubdivisionBasis(2), 600),
        maxSubdivisionPlotDiv(maxSubdivisionBasis(2), 400),
        maxSubdivisionPlotDiv(maxSubdivisionBasis(2), 300),
        maxSubdivisionPlotDiv(maxSubdivisionBasis(2), 200),
        maxSubdivisionPlotDiv(maxSubdivisionBasis(2), 100)
      )),
      onDemand("Zombies model", _ => {
        //TODO requires the actual subdivisions
        val basis = MultiScaleBasis(2, 32, 2)
        val (discoveredPoints, replications) = Data.pseZombiesData.map(_.drop(4)).map(v => (v.take(2), v.takeRight(1).head.toInt)).unzip
        val pointSet = new PointSet(discoveredPoints)
        val Subdivision = basis.subdivision
        val discoveredBoxes = pointSet.spaceNormalizedOutputs.map(_ * basis.subdivision).map(
          _
            .map(_.floor.toInt)
            .map {
              case Subdivision => basis.subdivision - 1
              case s => s
            }
        )
        computePlotDiv(basis, discoveredBoxes, size, replications)
      })
    )
  }

  val elementDemo: Demo = new Demo {
    def title: String = "PSE multi-scale"

    def code: String = sc.source

    def element: HtmlElement = sc.value
  }

}
