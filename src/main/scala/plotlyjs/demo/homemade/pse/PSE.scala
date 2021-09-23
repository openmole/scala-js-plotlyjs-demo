package plotlyjs.demo.homemade.pse

import com.raquo.airstream.state.Var
import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.openmole.plotlyjs.PlotMode.lines
import org.openmole.plotlyjs.PlotlyImplicits._
import org.openmole.plotlyjs.ShapeType.rect
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import org.scalajs.dom.html
import plotlyjs.demo.homemade.api.Data.Outcome
import plotlyjs.demo.homemade.api.PSE.{PSEDimension, PSEDisplay}
import plotlyjs.demo.homemade.utils.VectorColor._
import plotlyjs.demo.homemade.utils.IntVectors
import plotlyjs.demo.homemade.utils.IntVectors._
import plotlyjs.demo.homemade.utils.Utils.{ExtraTraceManager, resetViewButton}
import plotlyjs.demo.homemade.utils.Utils.ExtraTraceManager.ExtraTracesRef
import plotlyjs.demo.homemade.utils.Vectors._
import scaladget.bootstrapnative.bsn.{containerFluid, row}

import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.math.{ceil, max}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.JSRichIterableOnce

object PSE {

  def subdivisionIndexOf(dimensions: Seq[PSEDimension], outcome: Outcome): IntVector = {
    outcome.outputs.zip(dimensions).map { case (o, d) => d.bounds.lastIndexWhere(_ <= o.value) }
  }

  def sliceToString(dimensions: Seq[PSEDimension], basis: MultiScaleBasis, sliceIndex: IntVector): String = {
    sliceIndex.zipWithIndex.map { case (c, i) => dimensions(i).name + " [" + (c + 1) + "]" }.reduceOption(_ + ", " + _).getOrElse("")
  }

  def plot(dimensions: Seq[PSEDimension], basis: MultiScaleBasis, discovered: Seq[Outcome], pseDisplay: PSEDisplay, sliceOption: Option[IntVector] = None): ReactiveHtmlElement[html.Div] = {

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

    def sliceVertices(sliceIndex: IntVector): (Vector, Vector, Vector, Vector) = {
      val s0 = (0.0 at basis.destinationDimension) ++ sliceIndex.vector
      val sx = s0 + (0.0 at basis.sourceDimension).replace(0, basis.subdivisions(0))
      val sy = s0 + (0.0 at basis.sourceDimension).replace(1, basis.subdivisions(1))
      val sxy = s0 + (0.0 at basis.sourceDimension).replace(0, basis.subdivisions(0)).replace(1, basis.subdivisions(1))
      (s0, sx, sy, sxy)
    }

    val customLine = line
      .width(1)
      .color(0.5 at 4)

    val (frameShapeSeqSeq, hitboxDataSeq) = {

      val sliceIndexSeq = {
        val sliceSpaceDimension = basis.sourceDimension - basis.destinationDimension
        if (sliceSpaceDimension == 0) {
          Seq(Seq())
        } else {
          IntVectors.vectorIndices(basis.subdivisions.drop(basis.destinationDimension)).toSeq
        }
      }

      sliceIndexSeq.map { sliceIndex =>
        val (s0, sx, sy, sxy) = sliceVertices(sliceIndex)

        val frameShape = {
          val points = Seq(s0, sxy).map(basis.transform)
          val (x0, y0) = (points(0)(0), points(0)(1))
          val (x1, y1) = (points(1)(0), points(1)(1))
          Shape
            .`type`(rect)
            .x0(x0).x1(x1).y0(y0).y1(y1)
            .line(customLine)
            ._result
        }

        val gridShapeSeq = if (basis.sourceDimension != 2) Seq() else {
          val lowSBound = 0
          val highSBound = basis.subdivisions
          (lowSBound + 1 until highSBound(1)).map(s => {
            val p0 = basis.transform(s0.replace(0, lowSBound).replace(1, s))
            val p1 = basis.transform(s0.replace(0, highSBound(0)).replace(1, s))
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
              val p0V = basis.transform(s0.replace(0, s).replace(1, lowSBound))
              val p1V = basis.transform(s0.replace(0, s).replace(1, highSBound(1)))
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

        val hitboxData = {
          val points = Seq(s0, sx, sxy, sy).map(basis.transform)
          val coordinates = points.transpose
          scatter
            .x(coordinates(0).toJSArray)
            .y(coordinates(1).toJSArray)
            .setMode(lines)
            .line(line
              .width(0)
              .color(1 at 3) //TODO remove ?
            )
            .fill("toself")
            .fillcolor((0 at 3).opacity(0.0).toOMColor.toJS.toString) //TODO fillcolor(ColorType)
            .hoverinfo("text")
            .text(sliceToString(dimensions, basis, sliceIndex) + " – click to zoom")
            ._result
        }

        (frameShape +: gridShapeSeq, hitboxData)
      }.unzip
    }
    val frameShapeSeq = frameShapeSeqSeq.flatten

    lazy val boundsAnnotationSeq = {
      val annotationMinimumSize = 16 + 8

      def step(i: Int) = {
        val subdivisionSize = pseDisplay.size.toDouble / basis.subdivisions(i)
        ceil(annotationMinimumSize / subdivisionSize)
      }

      (basis.sourceDimension - basis.destinationDimension until basis.sourceDimension)
        .filter(i => basis.scaleIndex(i) == basis.maxScaleIndex)
        .flatMap(i => {
          val destinationAxis = basis.axis(i)
          val destinationScaleIndex = basis.scaleIndex(i)
          (if(basis.sourceDimension <= 2) 0 to basis.subdivisions(i) else 0 until basis.subdivisions(i))
            .filter(s => s % step(i) == 0 /* || s == basis.subdivision*/)
            .map(s => {
              val pixelToPlot = basis.totalSize(i) / pseDisplay.size.toDouble

              val point = (if(basis.sourceDimension <= 2) {
                basis.transform((0.0 at basis.sourceDimension).replace(i, s))
              } else {
                val iSubScale = i - basis.destinationDimension
                (basis.transform((0.0 at basis.sourceDimension).replace(i, s))
                  + basis.transform((0.0 at basis.sourceDimension).replace(i, s).replace(iSubScale, basis.subdivisions(iSubScale)))
                  )/2
              })
                .add({
                  val margin = 32 * pixelToPlot
                  val adjustmentFactor = destinationScaleIndex match {
                    case 0 => 1
                    case 1 => 0.5
                    case _ => 1
                  }
                  (-(margin * adjustmentFactor) * (destinationScaleIndex + 1) at basis.destinationDimension).replace(destinationAxis, 0)
                })
              val text = if(basis.sourceDimension <= 2) dimensions(i).bounds(s).toString /*TODO deal with scientific notation*/ else (s + 1).toString
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
                  .text(dimensions(i).name)
                  .showarrow(false)
                  ._result
                case 1 => Annotation
                  .xref("paper").yref("paper")
                  .x(0).xanchor("left")
                  .y(1).yanchor("top")
                  .text(dimensions(i).name)
                  .showarrow(false)
                  ._result
                case _ => Annotation.showarrow(false)._result
              }
            }
        })
    }

    val plotDiv = div()
    val plotDataSeq = if (basis.sourceDimension <= 2) Seq(scatter._result) else hitboxDataSeq
    val layout = {
      val subdivisionToPixel = pseDisplay.size / max(basis.size(0), basis.size(1))
      val topMargin = 32 + 8
      val internalBottomMargin = 64
      Layout
        .title("PSE" + (if (basis.stretched) " stretched" else "") + sliceOption.map(slice => " slice – " + sliceToString(dimensions, basis, slice)).getOrElse(""))
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
        )
        .yaxis(axis
          .scaleanchor("x")
          .visible(false)
        )
        .shapes((discoveredShapeSeq ++ frameShapeSeq).toJSArray)
        .annotations(boundsAnnotationSeq.toJSArray)
        .hovermode("closest")
        .dragmode(false)
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
      if(sliceOption.isEmpty) {
        config = config.modeBarButtonsToAdd(js.Array(resetViewButton(plotDiv, layout)))
      }
      config
    }
    Plotly.newPlot(plotDiv.ref, plotDataSeq.toJSArray, layout, config._result)

    if(sliceOption.isEmpty) {
      val zoom = Var(div())
      val extraTraceManager = new ExtraTraceManager(plotDiv, plotDataSeq.size)
      val focusedFrameRef = ExtraTraceManager.nullRef
      plotDiv.ref.on("plotly_click", pointsData => {
        val curveNumber = pointsData.points(0).curveNumber
        val sliceIndex = basis.sourceDimension match {
          case 3 =>
            Seq(curveNumber)
          case 4 =>
            val xSlice = curveNumber % basis.subdivisions(basis.destinationDimension)
            val ySlice = curveNumber / basis.subdivisions(basis.destinationDimension)
            Seq(xSlice, ySlice)
        }

        val focusedFrameData = {
          val (s0, sx, sy, sxy) = sliceVertices(sliceIndex)
          val points = Seq(s0, sx, sxy, sy, s0).map(basis.transform)
          val coordinates = points.transpose
          scatter
            .x(coordinates(0).toJSArray)
            .y(coordinates(1).toJSArray)
            .setMode(lines)
            .line(customLine
              .width(2)
              .color(0 at 3)
            )
            .hoverinfo("skip")
            ._result
        }
        extraTraceManager.updateTraces(focusedFrameRef, Some(Seq(focusedFrameData)))

        val filteredDiscovered = discovered.filter(outcome => subdivisionIndexOf(dimensions, outcome).takeRight(sliceIndex.vector.dimension).equals(sliceIndex))
        zoom.set(plot(
          dimensions,
          basis.copy(sourceDimension = basis.destinationDimension),
          filteredDiscovered,
          pseDisplay,
          Some(sliceIndex)
        ))
      })
      plotDiv.ref.on("plotly_relayout", () => {
        extraTraceManager.deleteAllTraces()
        zoom.set(div())
      })
      if(basis.sourceDimension == 4 && basis.subdivisions.reverse(1) <= basis.subdivisions.reverse(0)) {
        div(containerFluid,
          div(row, //The container must be wide enough.
            plotDiv,
            child <-- zoom.signal
          )
        )
      } else {
        div(containerFluid, justifyContent:="center", //not effective
          plotDiv,
          child <-- zoom.signal
        )
      }
    } else {
      plotDiv
    }
  }

  def plotAPI(dimensions: Seq[PSEDimension], outcomes: Seq[Outcome], pseDisplay: PSEDisplay): ReactiveHtmlElement[html.Div] = {
    plot(
      dimensions,
      MultiScaleBasis(dimensions.size, dimensions.map(_.bounds.size - 1), 2),
      outcomes,
      pseDisplay
    )
  }

}

/*
eventHandler

Uncaught TypeError: array is undefined
    $p_Lplotlyjs_demo_homemade_pareto_Pareto$__eventHandler$1__Lorg_openmole_plotlyjs_PointsData__Z__Z__Z__Z__sci_Seq__I__I__Lplotlyjs_demo_homemade_pareto_SnowflakeBasis__sci_IndexedSeq__Lplotlyjs_demo_homemade_pareto_PointPlotter__sr_ObjectRef__sr_ObjectRef__Lplotlyjs_demo_homemade_utils_Utils$ExtraTraceManager__Lcom_raquo_airstream_state_Var__V http://localhost:63342/scala-js-plotlyjs-demo/js/demoplotly.js:51630
    plotlyjs$demo$homemade$pareto$Pareto$$$anonfun$plot$29__Lorg_openmole_plotlyjs_PointsData__Lplotlyjs_demo_homemade_utils_Utils$SkipOnBusy__Lplotlyjs_demo_homemade_api_Pareto$ParetoDisplay__sci_Seq__I__I__Lplotlyjs_demo_homemade_pareto_SnowflakeBasis__sci_IndexedSeq__Lplotlyjs_demo_homemade_pareto_PointPlotter__sr_ObjectRef__sr_ObjectRef__Lplotlyjs_demo_homemade_utils_Utils$ExtraTraceManager__Lcom_raquo_airstream_state_Var__V http://localhost:63342/scala-js-plotlyjs-demo/js/demoplotly.js:51969
    ParetoDisplay__Lcom_raquo_laminar_nodes_ReactiveHtmlElement http://localhost:63342/scala-js-plotlyjs-demo/js/demoplotly.js:51943
    f http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:13
    emit http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:13
    emit http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    hover http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    hover http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    l http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    timer http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    setTimeout handler*[760]</r.throttle http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    hover http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    onmousemove http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    initInteractions http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    initInteractions http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    syncOrAsync http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    plot http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    redraw http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    t http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    addTraces__sci_Seq__V http://localhost:63342/scala-js-plotlyjs-demo/js/demoplotly.js:53091
    $p_Lplotlyjs_demo_homemade_pareto_Pareto$__eventHandler$1__Lorg_openmole_plotlyjs_PointsData__Z__Z__Z__Z__sci_Seq__I__I__Lplotlyjs_demo_homemade_pareto_SnowflakeBasis__sci_IndexedSeq__Lplotlyjs_demo_homemade_pareto_PointPlotter__sr_ObjectRef__sr_ObjectRef__Lplotlyjs_demo_homemade_utils_Utils$ExtraTraceManager__Lcom_raquo_airstream_state_Var__V http://localhost:63342/scala-js-plotlyjs-demo/js/demoplotly.js:51666
    plotlyjs$demo$homemade$pareto$Pareto$$$anonfun$plot$29__Lorg_openmole_plotlyjs_PointsData__Lplotlyjs_demo_homemade_utils_Utils$SkipOnBusy__Lplotlyjs_demo_homemade_api_Pareto$ParetoDisplay__sci_Seq__I__I__Lplotlyjs_demo_homemade_pareto_SnowflakeBasis__sci_IndexedSeq__Lplotlyjs_demo_homemade_pareto_PointPlotter__sr_ObjectRef__sr_ObjectRef__Lplotlyjs_demo_homemade_utils_Utils$ExtraTraceManager__Lcom_raquo_airstream_state_Var__V http://localhost:63342/scala-js-plotlyjs-demo/js/demoplotly.js:51969
    ParetoDisplay__Lcom_raquo_laminar_nodes_ReactiveHtmlElement http://localhost:63342/scala-js-plotlyjs-demo/js/demoplotly.js:51943
    f http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:13
    emit http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:13
    emit http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    hover http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    hover http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    l http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    timer http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    setTimeout handler*[760]</r.throttle http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    hover http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    onmousemove http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    initInteractions http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    initInteractions http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    syncOrAsync http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    plot http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    redraw http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    t http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    addTraces__sci_Seq__V http://localhost:63342/scala-js-plotlyjs-demo/js/demoplotly.js:53091
    $p_Lplotlyjs_demo_homemade_pareto_Pareto$__eventHandler$1__Lorg_openmole_plotlyjs_PointsData__Z__Z__Z__Z__sci_Seq__I__I__Lplotlyjs_demo_homemade_pareto_SnowflakeBasis__sci_IndexedSeq__Lplotlyjs_demo_homemade_pareto_PointPlotter__sr_ObjectRef__sr_ObjectRef__Lplotlyjs_demo_homemade_utils_Utils$ExtraTraceManager__Lcom_raquo_airstream_state_Var__V http://localhost:63342/scala-js-plotlyjs-demo/js/demoplotly.js:51666
    plotlyjs$demo$homemade$pareto$Pareto$$$anonfun$plot$29__Lorg_openmole_plotlyjs_PointsData__Lplotlyjs_demo_homemade_utils_Utils$SkipOnBusy__Lplotlyjs_demo_homemade_api_Pareto$ParetoDisplay__sci_Seq__I__I__Lplotlyjs_demo_homemade_pareto_SnowflakeBasis__sci_IndexedSeq__Lplotlyjs_demo_homemade_pareto_PointPlotter__sr_ObjectRef__sr_ObjectRef__Lplotlyjs_demo_homemade_utils_Utils$ExtraTraceManager__Lcom_raquo_airstream_state_Var__V http://localhost:63342/scala-js-plotlyjs-demo/js/demoplotly.js:51969
    ParetoDisplay__Lcom_raquo_laminar_nodes_ReactiveHtmlElement http://localhost:63342/scala-js-plotlyjs-demo/js/demoplotly.js:51943
    f http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:13
    emit http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:13
    emit http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    hover http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    hover http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    l http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    throttle http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    hover http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    onmousemove http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    initInteractions http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    initInteractions http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    syncOrAsync http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    plot http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    redraw http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    t http://localhost:63342/scala-js-plotlyjs-demo/js/deps.js:61
    addTraces__sci_Seq__V http://localhost:63342/scala-js-plotlyjs-demo/js/demoplotly.js:53091
    plotlyjs$demo$homemade$pareto$Pareto$$$anonfun$plot$38__Lorg_openmole_plotlyjs_PointsData__Lplotlyjs_demo_homemade_utils_Utils$SkipOnBusy__Lplotlyjs_demo_homemade_utils_Utils$ExtraTraceManager__sr_ObjectRef__sci_Seq__sr_ObjectRef__V http://localhost:63342/scala-js-plotlyjs-demo/js/demoplotly.js:51992
    ParetoDisplay__Lcom_raquo_laminar_nodes_ReactiveHtmlElement http://localhost:63342/scala-js-plotlyjs-demo/js/demoplotly.js:51955
demoplotly.js:51630:8

 */
