package plotlyjs.demo.homemade.pse

import com.raquo.airstream.state.Var
import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.openmole.plotlyjs.PlotlyImplicits._
import org.openmole.plotlyjs.ShapeType.rect
import org.openmole.plotlyjs.all._
import org.openmole.plotlyjs._
import org.scalajs.dom.html
import plotlyjs.demo.homemade.api.Data.Outcome
import plotlyjs.demo.homemade.api.PSE.PSEDimension
import plotlyjs.demo.utils.Colors._
import plotlyjs.demo.utils.Utils.printCode
import plotlyjs.demo.utils.vector.IntVectors
import plotlyjs.demo.utils.vector.IntVectors._
import plotlyjs.demo.utils.vector.Vectors._

import scala.math.{ceil, max}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.JSRichIterableOnce

object PSE {

  def subdivisionIndexOf(dimensions: Seq[PSEDimension], outcome: Outcome): IntVector = {
    outcome.outputs.zip(dimensions).map { case (o, d) => d.bounds.lastIndexWhere(_ <= o.value) }
  }

  def sliceToString(basis: MultiScaleBasis, slice: IntVector): String = {
    slice.zipWithIndex.map { case (c, i) => s"d${i + basis.destinationDimension + 1} s$c" }.reduceOption(_ + ", " + _).getOrElse("")
  }

  def plot(dimensions: Seq[PSEDimension], basis: MultiScaleBasis, discovered: Seq[Outcome], size: Int, parentContentVarOption: Option[Var[ReactiveHtmlElement[html.Div]]] = None, sliceOption: Option[IntVector] = None): ReactiveHtmlElement[html.Div] = {

    val discoveredShapeSeq = discovered.map { outcome =>
      val box = subdivisionIndexOf(dimensions, outcome).vector
      val b0 = box
      val b1 = b0 + (0.0 at basis.sourceDimension).replace(0, 1.0).replace(1, 1.0)
      val points = Seq(b0, b1).map(basis.transform)
      val coordinates = points.transpose
      val replication = outcome.samples.getOrElse(100) / 100.0
      val offset = 0.2
      Shape
        .`type`(rect)
        .x0(coordinates(0)(0))
        .x1(coordinates(0)(1))
        .y0(coordinates(1)(0))
        .y1(coordinates(1)(1))
        .line(line
          .width(0)
        )
        .fillcolor(Seq(1.0, 0.0, 0.0).opacity(offset + replication * (1 - offset)))
        ._result
    }

    val (frameShapeSeqSeq, hitboxDataSeq) = {
      val frameSeq = {
        val sliceSpaceDimension = basis.sourceDimension - basis.destinationDimension
        if (sliceSpaceDimension == 0) {
          Seq(Seq())
        } else {
          IntVectors.vectorIndices(basis.subdivisions.drop(basis.destinationDimension)).toSeq
        }
      }
      val customLine = line
        .width(1)
        .color(0.5 at 4)
      frameSeq
        .map((0.0 at basis.destinationDimension) ++ _.vector)
        .map { frame =>
          val s0 = frame
          val s1 = s0 + (0.0 at basis.sourceDimension).replace(0, basis.subdivisions(0)).replace(1, basis.subdivisions(1))
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
            val highSBound = basis.subdivisions
            (lowSBound + 1 until highSBound(1)).map(s => {
              val p0 = basis.transform(frame.replace(0, lowSBound).replace(1, s))
              val p1 = basis.transform(frame.replace(0, highSBound(0)).replace(1, s))
              Shape
                .`type`("line")
                .x0(p0(0))
                .y0(p0(1))
                .x1(p1(0))
                .y1(p1(1))
                .line(customLine)
                ._result
              }) ++ (lowSBound + 1 until highSBound(0))
              .map(s => {
                val p0V = basis.transform(frame.replace(0, s).replace(1, lowSBound))
                val p1V = basis.transform(frame.replace(0, s).replace(1, highSBound(1)))
                Shape
                  .`type`("line")
                  .x0(p0V(0))
                  .y0(p0V(1))
                  .x1(p1V(0))
                  .y1(p1V(1))
                  .line(customLine)
                  ._result
              })
          }
          val hitboxData = scatter
            .x(Seq((x0 + x1) / 2.0).toJSArray)
            .y(Seq((y0 + y1) / 2.0).toJSArray)
            .marker(marker
              //.size(size / (basis.subdivisions.sum.toDouble / basis.subdivisions.size) * 0.5)
              .symbol(square)
              .color(Seq(0.0, 1.0, 0.0))
              .opacity(1.0)
            )
            .hoverinfo("text")
            .text(sliceToString(basis, frame.drop(basis.destinationDimension)) + " – click to zoom")
            ._result
          (frameShape +: gridShapeSeq, hitboxData)
        }.unzip
    }
    val frameShapeSeq = frameShapeSeqSeq.flatten

    lazy val boundsAnnotationSeq = {
      val annotationMinimumSize = 16 + 8

      def step(i: Int) = {
        val subdivisionSize = size.toDouble / basis.subdivisions(i)
        ceil(annotationMinimumSize / subdivisionSize)
      }

      (basis.sourceDimension - basis.destinationDimension until basis.sourceDimension)
        .filter(i => basis.scaleIndex(i) == basis.maxScaleIndex)
        .flatMap(i => {
          val destinationAxis = basis.axis(i)
          val destinationScaleIndex = basis.scaleIndex(i);
          (0 until basis.subdivisions(i))
            .filter(s => s % step(i) == 0 /* || s == basis.subdivision*/)
            .map(s => {
              val pixelToPlot = basis.totalSize(i) / size.toDouble

              val point = ((basis.transform((0.0 at basis.sourceDimension).replace(i, s)) + {
                if (basis.sourceDimension <= 2) {
                  basis.transform((0.0 at basis.sourceDimension).replace(i, s + 1))
                } else {
                  val iSubScale = i - basis.destinationDimension
                  basis.transform((0.0 at basis.sourceDimension).replace(i, s).replace(iSubScale, basis.subdivisions(iSubScale)))
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
                  .text("d" + (i + 1))
                  .showarrow(false)
                  ._result
                case 1 => Annotation
                  .xref("paper").yref("paper")
                  .x(0).xanchor("left")
                  .y(1).yanchor("top")
                  .text("d" + (i + 1))
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
        //.width(basis.size(0) * subdivisionToPixel)
        //.height(basis.size(1) * subdivisionToPixel + topMargin + internalBottomMargin)
        .autosize(true)
        /*
        .margin(Margin
          .t(topMargin)
          .l(0).r(0)
          .b(0)
        )
        */
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
            val xSlice = curveNumber % basis.subdivisions(basis.destinationDimension)
            val ySlice = curveNumber / basis.subdivisions(basis.destinationDimension)
            Seq(xSlice, ySlice)
        }

        val filteredDiscovered = discovered.filter(outcome => subdivisionIndexOf(dimensions, outcome).takeRight(slice.vector.dimension).equals(slice))
        contentVar.set(plot(
          dimensions,
          basis.copy(sourceDimension = basis.destinationDimension),
          filteredDiscovered,
          size,
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

  def api(dimensions: Seq[PSEDimension], outcomes: Seq[Outcome]): ReactiveHtmlElement[html.Div] = {
    plot(
      dimensions,
      MultiScaleBasis(dimensions.size, dimensions.map(_.bounds.size - 1), 2),
      outcomes,
      800
    )
  }

}
