package plotlyjs.demo.homemade.pareto

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.openmole.plotlyjs.HoverMode.closest
import org.openmole.plotlyjs.PlotMode.lines
import org.openmole.plotlyjs.PlotlyImplicits.elToPlotlyElement
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all.{PlotMarkerAPI, _}
import org.scalajs.dom.html
import plotlyjs.demo.homemade.api.Data.Outcome
import plotlyjs.demo.homemade.api.Pareto.{Maximization, ParetoDisplay, ParetoObjective}
import plotlyjs.demo.homemade.pareto.SnowflakeBasis.{cartesianFromPolar, polarFromCartesian}
import plotlyjs.demo.homemade.utils.Utils.{ExtraTraceManager, SkipOnBusy, resetViewButton}
import plotlyjs.demo.homemade.utils.VectorColor
import plotlyjs.demo.homemade.utils.VectorColor._
import plotlyjs.demo.homemade.utils.Vectors._

import scala.math.Numeric.BigDecimalAsIfIntegral.abs
import scala.math.ceil
import scala.scalajs.js
import scala.scalajs.js.JSConverters.JSRichIterableOnce
import scala.scalajs.js.Object.entries

object Pareto {

  def plot(objectives: Seq[ParetoObjective], outcomes: Seq[Outcome], paretoDisplay: ParetoDisplay): ReactiveHtmlElement[html.Div] = {

    //Style
    val backgroundColor = 245.toDouble/255 at 3

    val opacity = 0.75

    val circlePointsCount = 8

    val circleMargin = 0.025

    val minMarkerRadius = 0.02
    val maxMarkerRadius = 0.07
    val markerLineWidth = 2
    val markerLineColor = 0 at 3
    val markerLine = line
      .width(markerLineWidth)
      .color(markerLineColor)

    val arcPointsCount = 4
    val arcMargin = 0.1
    val arcWidth = 4
    val arcLine = line
      .width(arcWidth)

    val focusWidth = arcWidth
    val focusColor = markerLineColor.opacity(opacity)
    val focusLine = markerLine
      .width(focusWidth)
      .color(focusColor)

    val tickRadius = 0.05
    val tickLineWidth = arcWidth
    val tickLineColor = focusColor
    val tickLine = line
      .width(tickLineWidth)
      .color(tickLineColor)
    //

    val dimension = objectives.size
    val basis = new SnowflakeBasis(dimension)

    val spaceNormalObjectives = (0 until dimension).map((0 at dimension).replace(_, 1))
    val cartesianObjectives = spaceNormalObjectives.map(basis.transform)
    val polarObjectives = cartesianObjectives.map(polarFromCartesian)
    val colors = polarObjectives.map(vector => Seq(((vector(1) + 360) % 360) / 360, 0.9, 0.45).fromHSLtoRGB.opacity(opacity))

    val axisShapeSeq = cartesianObjectives.zipWithIndex.map { case (o, i) =>
      Shape
        .`type`("line")
        .x0(0).y0(0)
        .x1(o(0)).y1(o(1))
        .line(line
          .color(colors(i))
        )
        .layer("below")
        ._result
    }

    val pointPlotter = PointPlotter(
      objectives.map(_.optimizationType),
      outcomes.map(_.outputs.map(_.value))
    )

    case class RichPoint(outcome: Outcome, index: Int, plotOutput: Vector, point: Vector, radius: Double, color: VectorColor.Color)
    val richPoints = outcomes.zipWithIndex.map { case(outcome: Outcome, index) =>
      val plotOutput = pointPlotter.plotOutputs(index)
      val point = basis.transform(plotOutput)
      val replication = outcome.samples.getOrElse(100) / 100.0
      val radius = minMarkerRadius + replication * (maxMarkerRadius - minMarkerRadius)
      val color = colors(plotOutput.zipWithIndex.maxBy(_._1)._2)
      RichPoint(outcome, index, plotOutput, point, radius, color)
    }

    val plotRadius = richPoints.map(rp => Seq(rp.point(0), rp.point(1)).norm).max * 1.1

    val backgroundShape =
      Shape
        .`type`("circle")
        .xref("x")
        .yref("y")
        .x0(-plotRadius)
        .y0(-plotRadius)
        .x1(plotRadius)
        .y1(plotRadius)
        .line(line
          .width(0)
        )
        .fillcolor(backgroundColor)
        .layer("below")
        ._result

    val legendAnnotationSeq = cartesianObjectives.zipWithIndex.map { case (o, i) =>
      val textPosition = 1.1 * plotRadius * o
      val angle = polarFromCartesian(textPosition).angle
      val (a1, a2, a3, a4) = (-135, -45, 45, 135)
      val (a5, a6) = (-90, 90)
      Annotation
        .x(textPosition(0))
        .y(textPosition(1))
        .text((if(objectives(i).optimizationType == Maximization) "-" else "") + objectives(i).name)
        //.xanchor(if(a2 <= angle && angle < a3) "left" else if(a4 <= angle || angle < a1) "right" else "center")
        .xanchor(if(a5 <= angle && angle < a6) "left" else "right")
        .yanchor(if(a1 <= angle && angle < a2) "top" else if(a3 <= angle && angle < a4) "bottom" else "center")
        .showarrow(false)
        ._result
    }

    def closedCircle(center: Vector, radius: Double) = {
      val circleBasis = new SnowflakeBasis(circlePointsCount)
      val points = (0 until circlePointsCount).map { i =>
        circleBasis.transform((0.0 at circlePointsCount).replace(i, radius)).add(center)
      }
      points :+ points.head
    }

    val paretoFrontDataSeq = richPoints.flatMap { rp =>
      val points = closedCircle(rp.point, rp.radius)
      val coordinates = points.transpose
      val superMarker = scatter
        .x(coordinates(0).toJSArray)
        .y(coordinates(1).toJSArray)
        .setMode(lines)
        .line(markerLine
          .shape("spline")
        )
        .fill("toself")
        .fillcolor(rp.color.toOMColor.toJS.toString)
        .hoverinfo("skip")
        ._result
      val hitbox = scatter
        .x(js.Array(rp.point(0)))
        .y(js.Array(rp.point(1)))
        .marker(marker
          .color(1.0 at 3)
          .opacity(0.0)
        )
        .hoverinfo("text")
        .text("click me") //or changes color on hover ?
        .customdata(js.Array(rp.index.toString))
        ._result
      Seq(superMarker, hitbox)
    }

    //Display
    val plotDiv = div()
    val dataSeq = paretoFrontDataSeq
    val layout = {
      val shapeSeq = (if (dimension == 2) None else Some(backgroundShape)) ++ axisShapeSeq
      val annotationSeq = legendAnnotationSeq
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
        .dragmode(false)
        ._result
    }
    val config = Config
      .modeBarButtonsToRemove(Seq(
        "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
        "hoverClosestCartesian", "hoverCompareCartesian",
        "toggleSpikelines",
      ).toJSArray)
      .modeBarButtonsToAdd(js.Array(resetViewButton(plotDiv, layout)))
      ._result
    Plotly.newPlot(plotDiv.ref, dataSeq.toJSArray, layout, config)
    //

    //Dynamic display
    var selectedRichPoint: Option[RichPoint] = None
    val extraTraceManager = new ExtraTraceManager(plotDiv, dataSeq.size)
    val snowflakeRef = ExtraTraceManager.nullRef
    val focusRef = ExtraTraceManager.nullRef
    val ticksRef = ExtraTraceManager.nullRef
    val improvementsRef = ExtraTraceManager.nullRef
    val componentSumRef = ExtraTraceManager.nullRef

    val rawOutputCoordinates = Var(div(""))

    class Action
    object Add extends Action
    object Remove extends Action
    object Toggle extends Action
    object NoAction extends Action
    def eventHandler(pointsData: PointsData,
                     snowflake: Action = NoAction,
                     improvementHelp: Action = NoAction,
                     componentSum: Action = NoAction,
                    ): Unit = {
      if(js.isUndefined(pointsData.points)) return
      if(pointsData.points.isEmpty) return
      val tuple2Option = entries(pointsData.points.head).find { t => t._1 == "customdata" }
      if(tuple2Option.isEmpty) return
      if(js.isUndefined(tuple2Option.get._2)) return
      val stringCustomData = tuple2Option.get._2.toString
      val intCustomData = try {
        stringCustomData.toInt
      } catch {
        case _: NumberFormatException => return
      }

      val rp = richPoints(intCustomData)

      lazy val componentSumDataSeq = {
        var cartesianEnd = 0.0 at 2
        rp.plotOutput.zipWithIndex.sortBy({ case (c, _) => abs(c) }).reverse.map(_._2).map(i => {
          val cartesianComponentVector = basis.component(rp.plotOutput, i)
          val cartesianBegin = cartesianEnd
          cartesianEnd = cartesianBegin + cartesianComponentVector
          val sumPolarCoordinates = Seq(cartesianBegin, cartesianEnd).transpose
          scatter
            .x(sumPolarCoordinates(0).toJSArray)
            .y(sumPolarCoordinates(1).toJSArray)
            .setMode(lines)
            .line(line
              .dash("dot")
              .color(colors(i))
            )
            .hoverinfo("skip")
            ._result
        })
      }

      lazy val snowflakeDataSeq = {
        (0 until dimension).map(i => {
          val componentVector = basis.component(rp.plotOutput, i)
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

      def focusData(rp: RichPoint) = {
        val points = closedCircle(rp.point, rp.radius + circleMargin)
        val coordinates = points.transpose
        scatter
          .x(coordinates(0).toJSArray)
          .y(coordinates(1).toJSArray)
          .setMode(lines)
          .line(focusLine
            .shape("spline")
          )
          .hoverinfo("skip")
          ._result
      }

      def ticksDataSeq(rp: RichPoint) = {
        (0 until dimension).flatMap(i => {
          val cartesianComponentVector = basis.component(rp.plotOutput, i)
          val polarComponentVector = polarFromCartesian(cartesianComponentVector)
          if(polarComponentVector.radius == 0) {
            None
          } else {
            val segment = Seq(-90, +90).map { angleDelta =>
              cartesianFromPolar(
                Seq(tickRadius, polarComponentVector.angle + angleDelta)
              ).add(cartesianComponentVector)
            }
            val coordinates = segment.transpose
            Some(scatter
              .x(coordinates(0).toJSArray)
              .y(coordinates(1).toJSArray)
              .setMode(lines)
              .line(tickLine)
              .hoverinfo("skip")
              ._result
            )
          }
        })
      }

      def improvementsDataSeq(rp: RichPoint) = {
        val improvementParetoFront = pointPlotter.plotOutputs.map { v =>
          (v - rp.plotOutput).map(_ > 0)
        }
        improvementParetoFront.zipWithIndex.flatMap { case (improvementVector, index) =>
          val rp = richPoints(index)

          val polygon = (0 until dimension).map { i =>
            cartesianFromPolar(
              polarFromCartesian(
                basis.transform((0.0 at dimension).replace(i, 1))
              ).add(Seq(0.0, - 360.0 / dimension / 2))
            )
          }
          polygon.zip(polygon.drop(1) ++ polygon.take(1)).zipWithIndex.flatMap { case ((p1, p2), i) =>
            if(improvementVector(i)) {
              val points = (0 until arcPointsCount).map(_.toDouble / (arcPointsCount - 1)).map(arcMargin + _ * (1 - 2*arcMargin)).map { alpha =>
                cartesianFromPolar(Seq(rp.radius + circleMargin, polarFromCartesian((1 - alpha) * p1 + alpha * p2).angle))
              }.map(_.add(rp.point))
              val coordinates = points.transpose
              Some(scatter
                .x(coordinates(0).toJSArray)
                .y(coordinates(1).toJSArray)
                .setMode(lines)
                .line(arcLine
                  .color(colors(i))
                  .shape("spline")
                )
                .hoverinfo("skip")
                ._result)
            } else {
              None
            }
          }
        }
      }

      val updateImprovements = {
        improvementHelp match {
          case Toggle => {
            if(rp != selectedRichPoint.orNull) {
              selectedRichPoint = Some(rp)
              Add
            } else {
              selectedRichPoint = None
              Remove
            }
          }
          case _ => NoAction
        }
      }

      updateImprovements match {
        case NoAction =>
        case _ => {
          extraTraceManager.updateTraces(improvementsRef, selectedRichPoint.map(improvementsDataSeq))
          extraTraceManager.updateTraces(focusRef, selectedRichPoint.map(srp => Seq(focusData(srp))))
        }
      }

      snowflake match {
        case Add => extraTraceManager.updateTraces(snowflakeRef, Some(snowflakeDataSeq))
        case Remove => extraTraceManager.updateTraces(snowflakeRef, None)
        case _ =>
      }

      updateImprovements match {
        case NoAction =>
        case _ => extraTraceManager.updateTraces(ticksRef, selectedRichPoint.map(ticksDataSeq))
      }

      componentSum match {
        case Add => extraTraceManager.updateTraces(componentSumRef, Some(componentSumDataSeq))
        case Remove => extraTraceManager.updateTraces(componentSumRef, None)
        case _ =>
      }

      val textDiv = div()
      textDiv.ref.innerHTML = "Model output :<br>" + (rp.outcome.outputs.map(_.value).zipWithIndex map { case (c, i) => s"o${i + 1} : $c" }).mkString("<br>")
      rawOutputCoordinates.set(textDiv)
    }

    val skipOnBusy = new SkipOnBusy
    plotDiv.ref.on("plotly_hover", pointsData => skipOnBusy.skipOnBusy("hover", () => {
      eventHandler(pointsData, snowflake = Add, componentSum = if(paretoDisplay.showPath) Add else NoAction)
    }))
    plotDiv.ref.on("plotly_unhover", pointsData => skipOnBusy.skipOnBusy("unhover", () => {
      eventHandler(pointsData, snowflake = Remove, componentSum = if(paretoDisplay.showPath) Remove else NoAction)
    }))
    plotDiv.ref.on("plotly_click", pointsData => skipOnBusy.skipOnBusy("click", () => {
      eventHandler(pointsData, improvementHelp = Toggle)
    }))
    plotDiv.ref.on("plotly_relayout", () => skipOnBusy.skipOnBusy("relayout", () => {
      extraTraceManager.deleteAllTraces()
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
