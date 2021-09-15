package plotlyjs.demo.homemade.pareto

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.openmole.plotlyjs.Color.rgb
import org.openmole.plotlyjs.HoverMode.closest
import org.openmole.plotlyjs.PlotMode.{lines, markers, markersAndText, text}
import org.openmole.plotlyjs.PlotlyImplicits.elToPlotlyElement
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all.{PlotMarkerAPI, _}
import org.scalajs.dom.html
import plotlyjs.demo.homemade.api.Data.Outcome
import plotlyjs.demo.homemade.api.Pareto.{Maximization, ParetoDisplay, ParetoObjective}
import plotlyjs.demo.homemade.pareto.PointPlotter.BetterPlot
import plotlyjs.demo.homemade.pareto.SnowflakeBasis.{cartesianFromPolar, polarFromCartesian}
import plotlyjs.demo.homemade.utils.VectorColor
import plotlyjs.demo.homemade.utils.VectorColor._
import plotlyjs.demo.homemade.utils.Utils.{ExtraTraceManager, SkipOnBusy}
import plotlyjs.demo.homemade.utils.Vectors._

import scala.math.Numeric.BigDecimalAsIfIntegral.abs
import scala.math.ceil
import scala.scalajs.js
import scala.scalajs.js.JSConverters.JSRichIterableOnce

object Pareto { //TODO no zoom, no useless button

  def plot(objectives: Seq[ParetoObjective], outcomes: Seq[Outcome], paretoDisplay: ParetoDisplay): ReactiveHtmlElement[html.Div] = {
    val dimension = objectives.size
    val downHalfDimension = dimension / 2
    val upHalfDimension = ceil(dimension / 2.0).toInt

    val basis = new SnowflakeBasis(dimension)

    val spaceNormalObjectives = (0 until dimension).map((0 at dimension).replace(_, 1))
    val cartesianObjectives = spaceNormalObjectives.map(basis.transform)
    val polarObjectives = cartesianObjectives.map(polarFromCartesian)
    val colors = polarObjectives.map(vector => Seq(((vector(1) + 360) % 360) / 360, 1.0, 0.75).fromHSLtoRGB)

    val axisShapeSeq = cartesianObjectives.zipWithIndex.map { case (o, i) =>
      Shape
        .`type`("line")
        .x0(0).y0(0)
        .x1(o(0)).y1(o(1))
        .line(line
          .color(colors(i).opacity(1.0))
        )
        .layer("below")
        ._result
    }

    val legendAnnotationSeq = cartesianObjectives.zipWithIndex.map { case (o, i) =>
      val textPosition = 1.1 * o
      val angle = polarFromCartesian(textPosition).angle
      val (a1, a2, a3, a4) = (-135, -45, 45, 135)
      Annotation
        .x(textPosition(0))
        .y(textPosition(1))
        .text((if(objectives(i).optimizationType == Maximization) "-" else "") + objectives(i).name)
        .xanchor(if(a2 <= angle && angle < a3) "left" else if(a4 <= angle || angle < a1) "right" else "center")
        .yanchor(if(a1 <= angle && angle < a2) "top" else if(a3 <= angle && angle < a4) "bottom" else "center")
        .showarrow(false)
        ._result
    }

    val pointPlotter = PointPlotter(
      objectives.map(_.optimizationType),
      outcomes.map(_.outputs.map(_.value)),
      BetterPlot.IsHigher
    )

    case class RichPoint(outcome: Outcome, index: Int, plotOutput: Vector, x: Double, y: Double, size: Double, color: VectorColor.Color)
    val offset = 0.2
    val richPoints = outcomes.zipWithIndex.map { case(outcome: Outcome, index) =>
      val plotOutput = pointPlotter.plotOutputs(index)
      val point = basis.transform(plotOutput)
      val replication = outcome.samples.getOrElse(100) / 100.0
      val size = 6 + replication * 16
      val color = (pointPlotter.betterPlot match {
        case BetterPlot.IsLower => colors(plotOutput.zipWithIndex.minBy(_._1)._2)
        case BetterPlot.IsHigher => colors(plotOutput.zipWithIndex.maxBy(_._1)._2)
      })//.opacity(0.5)//.opacity(offset + replication * (1 - offset))
      RichPoint(outcome, index, plotOutput, point(0), point(1), size, color)
    }

    val markerSize = 16
    val paretoFrontDataSeq = richPoints.map { p =>
      val replication = p.outcome.samples.getOrElse(100) / 100.0
      scatter
        .x(Seq(p.x).toJSArray)
        .y(Seq(p.y).toJSArray)
        .setMode(markers)
        .marker(marker
          .size(p.size)
          .symbol(circle)
          .color(p.color)
          .set(line
            .width(1)
            .color((0.5 at 3))
          )
        )
        .hoverinfo("text")
        .text("click me") //or changes color on hover ?
        .customdata(Seq(p.index.toString).toJSArray)
        ._result
    }

    val backgroundShape = {

      val radius = {
        //basis.transform((1 at upHalfDimension) ++ (0 at dimension - upHalfDimension)).norm //Leave space for the componentSum to display...
        richPoints.map(p => Seq(p.x, p.y).norm).max //... or fit the points.
      } * 1.1

      Shape
        .`type`("circle")
        .xref("x")
        .yref("y")
        .x0(-radius)
        .y0(-radius)
        .x1(radius)
        .y1(radius)
        .line(line.width(0))
        .fillcolor(rgb(245, 245, 245))
        .layer("below")
        ._result
    }

    //Display
    val plotDiv = div()
    val dataSeq = Seq[PlotData]()//paretoFrontDataSeq
    val shapeSeq = (if (dimension == 2) None else Some(backgroundShape)) ++ axisShapeSeq
    val annotationSeq = legendAnnotationSeq
    Plotly.newPlot(
      plotDiv.ref,
      dataSeq.toJSArray,
      Layout
        .title("Pareto")
        .width(paretoDisplay.size)
        .height(paretoDisplay.size)
        .xaxis(axis
          .visible(false)
        )
        .yaxis(axis
          .scaleanchor("x")
          .visible(false)
        )
        .showlegend(false)
        .shapes(shapeSeq.toJSArray)
        .annotations(annotationSeq.toJSArray)
        .hovermode(closest)
        ._result
    )
    //

    var currentReference: Option[RichPoint] = None

    val extraTraceManager = new ExtraTraceManager(plotDiv, dataSeq.size)
    val defaultParetoFrontDisplay = paretoFrontDataSeq
    var currentParetoFrontDisplay = defaultParetoFrontDisplay
    extraTraceManager.addTraces(currentParetoFrontDisplay)
    var currentSnowflakeReference = Seq[PlotData]()

    val rawOutputCoordinates = Var(div(""))

    def eventHandler(pointsData: PointsData,
                     coordinateSnowflake: Boolean = false,
                     componentSum: Boolean = false,
                     compromiseHelp: Boolean = false
                    ): Unit = {
      if(pointsData.points.isEmpty) return

      val richPoint = richPoints(pointsData.points.head.customdata.toInt)

      var plotDataSeq = Seq[PlotData]()

      if(componentSum) plotDataSeq = plotDataSeq ++ {
        var cartesianEnd = 0.0 at 2
        richPoint.plotOutput.zipWithIndex.sortBy({ case (c, _) => abs(c) }).reverse.map(_._2).map(i => {
          val cartesianComponentVector = basis.component(richPoint.plotOutput, i)
          val cartesianBegin = cartesianEnd
          cartesianEnd = cartesianBegin + cartesianComponentVector
          val sumPolarCoordinates = Seq(cartesianBegin, cartesianEnd).transpose
          scatter
            .x(sumPolarCoordinates(0).toJSArray)
            .y(sumPolarCoordinates(1).toJSArray)
            .setMode(lines)
            .line(line
              //.width(1)
              .dash("dot")
              .color(/*(0.5 at 3)*/ colors(i).opacity(0.5))
            )
            .hoverinfo("skip")
            ._result
        })
      }

      if(coordinateSnowflake) plotDataSeq = plotDataSeq ++ {
        (0 until dimension).map(i => {
          val componentVector = basis.component(richPoint.plotOutput, i)
          val coordinates = Seq(0.0 at 2, componentVector).transpose
          scatter
            .x(coordinates(0).toJSArray)
            .y(coordinates(1).toJSArray)
            .setMode(lines)
            .line(line
              .width(8)
              .color(colors(i))
            )
            .hoverinfo("skip")
            ._result
        })
      }

      val referenceColor = 0.5 at 3

      lazy val snowflakeReferenceDataSeq = {
        (0 until dimension).flatMap(i => {
          val cartesianComponentVector = basis.component(richPoint.plotOutput, i)
          val polarComponentVector = polarFromCartesian(cartesianComponentVector)
          if(polarComponentVector.radius == 0) return

          val l = 0.05
          val r = 0.01
          lazy val segment = Seq(-90, +90).map { angleDelta =>
            cartesianFromPolar(
              Seq(l, polarComponentVector.angle + angleDelta)
            ).add(cartesianComponentVector)
          }
          lazy val quad = Seq(-r, +r).map { radiusDelta =>
            Seq(-90, +90).map { angleDelta =>
              cartesianFromPolar(
                polarFromCartesian(
                  cartesianFromPolar(
                    Seq(l, polarComponentVector.angle + angleDelta)
                  ).add(cartesianComponentVector)
                ).add(Seq(radiusDelta, 0))
              )
            }
          }
          lazy val rectangularQuad = Seq(quad(0)(0), quad(1)(0), quad(1)(1), quad(0)(1))
          lazy val doubleTriangleQuad = Seq(quad(0)(0), quad(1)(0), quad(0)(1), quad(1)(1))
          lazy val closedQuad = rectangularQuad :+ rectangularQuad.head//doubleTriangleQuad :+ doubleTriangleQuad.head
          val coordinates = segment.transpose//closedQuad.transpose
          Some(scatter
            .x(coordinates(0).toJSArray)
            .y(coordinates(1).toJSArray)
            .setMode(lines)
            .line(line
              .width(1)
              .shape("spline")
              .color((0 at 3).opacity(0.5))
              //.color(colors(i))
            )
            //.fill("toself")
            //.fillcolor(/*colors(i)*/referenceColor.toOMColor.toJS.toString)
            //.fillcolor(colors(i).toOMColor.toJS.toString)
            .hoverinfo("skip")
            ._result
          )
        })
      }

      lazy val multiObjectiveCompromiseDataSeq = {
        val improvementParetoFront = pointPlotter.plotOutputs.map { v =>
          (v, (v - richPoint.plotOutput).count(pointPlotter.betterPlot match {
            case BetterPlot.IsLower => _ < 0
            case BetterPlot.IsHigher => _ > 0
          }))
        }
        val size = 16
        improvementParetoFront.zipWithIndex.flatMap { case ((point, count), index) => {
          val improvement = count.toDouble/dimension
          val coordinates = Seq(point).map(basis.transform).transpose
          val richPoint = richPoints(index)
          /*
          Seq(scatter
            .x(coordinates(0).toJSArray)
            .y(coordinates(1).toJSArray)
            .setMode(markers)
            .marker(
              if(count == 0) {
                marker
                  .opacity(0.0)
              } else {
                marker
                  .size(richPoint.size + improvement * size)
                  .symbol(circle.open)
                  .set(line
                    .width(1)
                  ) //TODO .line(
                  .color(0.5 at 3)
              }
            )
            .hoverinfo("skip")
            ._result
          })
          */

          /*
          val circles = 3
          var d = 0.0
          var seq = Seq[PlotData]()
          while(d < improvement) {
            seq = seq :+ scatter
              .x(coordinates(0).toJSArray)
              .y(coordinates(1).toJSArray)
              .marker(marker
                .size(richPoint.size + d * size)
                .symbol(circle.open)
                .set(line
                  .width(1)
                )
                .color(0.5 at 3)
              )
              .hoverinfo("skip")
              ._result
            d += 1.0/(circles + 1)
          }
          seq
          */

          val discs = (dimension - 1)
          var d = 0.0
          var seq = Seq[PlotData]()
          (1 to count).map { d =>
            val p = d.toDouble/dimension
            scatter
              .x(coordinates(0).toJSArray)
              .y(coordinates(1).toJSArray)
              .marker(marker
                .size(richPoint.size + p * size)
                .symbol(circle)
                .color(0.6 at 3)
                .opacity(1 - p)
              )
              .hoverinfo("skip")
              ._result
          }.reverse
          /*
          while(d < improvement) {
            seq = seq :+ scatter
              .x(coordinates(0).toJSArray)
              .y(coordinates(1).toJSArray)
              .marker(marker
                .size(richPoint.size + d * size)
                .symbol(circle)
                .color(0.6 at 3)
                .opacity(1 - d)
              )
              .hoverinfo("skip")
              ._result
            d += 1.0/(discs + 1)
          }
          seq.reverse
          */
        }}
      }

      if(compromiseHelp) {
        if(richPoint != currentReference.orNull) {
          currentReference = Some(richPoint)
          currentSnowflakeReference = snowflakeReferenceDataSeq
          currentParetoFrontDisplay = multiObjectiveCompromiseDataSeq ++ paretoFrontDataSeq

        } else {
          currentReference = None
          currentSnowflakeReference = Seq()
          currentParetoFrontDisplay = defaultParetoFrontDisplay
        }
      }

      extraTraceManager.deleteTraces()
      extraTraceManager.addTraces(currentParetoFrontDisplay)
      extraTraceManager.addTraces(plotDataSeq)
      extraTraceManager.addTraces(currentSnowflakeReference)

      val textDiv = div()
      textDiv.ref.innerHTML = "Model output :<br>" + (richPoint.outcome.outputs.map(_.value).zipWithIndex map { case (c, i) => s"o${i + 1} : $c" }).mkString("<br>")
      rawOutputCoordinates.set(textDiv)
    }

    val skipOnBusy = new SkipOnBusy
    plotDiv.ref.on("plotly_hover", pointsData => skipOnBusy.skipOnBusy("hover", () => {
      eventHandler(pointsData, coordinateSnowflake = true, componentSum = paretoDisplay.showPath)
    }))
    plotDiv.ref.on("plotly_unhover", pointsData => skipOnBusy.skipOnBusy("unhover", () => {
      eventHandler(pointsData)
    }))
    plotDiv.ref.on("plotly_click", pointsData => skipOnBusy.skipOnBusy("click", () => {
      eventHandler(pointsData, compromiseHelp = true)
    }))
    plotDiv.ref.on("plotly_relayout", _ => skipOnBusy.skipOnBusy("relayout", () => {
      extraTraceManager.deleteTraces()
      currentReference = None
      currentSnowflakeReference = Seq()
      currentParetoFrontDisplay = defaultParetoFrontDisplay
      extraTraceManager.addTraces(defaultParetoFrontDisplay)
    }))
    //

    div(
      plotDiv,
      div(child <-- rawOutputCoordinates.signal)
    )
  }

  def plotAPI(objectives: Seq[ParetoObjective], outcomes: Seq[Outcome], paretoDisplay: ParetoDisplay): ReactiveHtmlElement[html.Div] = {
    plot(objectives, outcomes, paretoDisplay)
  }

}
