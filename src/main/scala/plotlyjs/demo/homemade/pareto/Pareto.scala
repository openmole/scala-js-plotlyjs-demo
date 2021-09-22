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
import plotlyjs.demo.homemade.pareto.PointPlotter.BetterPlot
import plotlyjs.demo.homemade.pareto.SnowflakeBasis.{cartesianFromPolar, polarFromCartesian}
import plotlyjs.demo.homemade.utils.Utils.ExtraTraceManager.ExtraTracesRef
import plotlyjs.demo.homemade.utils.Utils.{ExtraTraceManager, SkipOnBusy}
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
    val dimension = objectives.size
    val downHalfDimension = dimension / 2
    val upHalfDimension = ceil(dimension / 2.0).toInt

    val basis = new SnowflakeBasis(dimension)

    val spaceNormalObjectives = (0 until dimension).map((0 at dimension).replace(_, 1))
    val cartesianObjectives = spaceNormalObjectives.map(basis.transform)
    val polarObjectives = cartesianObjectives.map(polarFromCartesian)
    val colors = polarObjectives.map(vector => Seq(((vector(1) + 360) % 360) / 360, 1.0, 0.5).fromHSLtoRGB.opacity(0.75))

    val customLine = line
      .width(2)
      .color(0 at 3)
    val backgroundColor = 245.toDouble/255 at 3

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

    case class RichPoint(outcome: Outcome, index: Int, plotOutput: Vector, point: Vector, replication: Double, radius: Double, size: Double, color: VectorColor.Color)
    val offset = 0.2
    val richPoints = outcomes.zipWithIndex.map { case(outcome: Outcome, index) =>
      val plotOutput = pointPlotter.plotOutputs(index)
      val point = basis.transform(plotOutput)
      val replication = outcome.samples.getOrElse(100) / 100.0
      val radius = 0.02 + replication * 0.05
      val size = 6 + replication * 16
      val color = (pointPlotter.betterPlot match {
        case BetterPlot.IsLower => colors(plotOutput.zipWithIndex.minBy(_._1)._2)
        case BetterPlot.IsHigher => colors(plotOutput.zipWithIndex.maxBy(_._1)._2)
      })//.opacity(0.5)//.opacity(offset + replication * (1 - offset))
      RichPoint(outcome, index, plotOutput, point, replication, radius, size, color)
    }

    val paretoFrontDataSeq = richPoints.flatMap { rp =>
      val circle = {
        val n = 12
        val circleBasis = new SnowflakeBasis(n)
        val points = (0 until n).map { i =>
          circleBasis.transform((0.0 at n).replace(i, rp.radius)).add(rp.point)
        }
        points :+ points.head
      }

      val points = circle
      val coordinates = points.transpose
      val superMarker = scatter
        .x(coordinates(0).toJSArray)
        .y(coordinates(1).toJSArray)
        .setMode(lines)
        .line(customLine
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

    val backgroundShape = {

      val radius = {
        //basis.transform((1 at upHalfDimension) ++ (0 at dimension - upHalfDimension)).norm //Leave space for the componentSum to display...
        richPoints.map(rp => Seq(rp.point(0), rp.point(1)).norm).max //... or fit the points.
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
        //.fillcolor(rgb(245, 245, 245))
        .fillcolor(backgroundColor)
        .layer("below")
        ._result
    }

    //Display
    val plotDiv = div()
    val dataSeq = paretoFrontDataSeq
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
        .dragmode(false)
        ._result,
      Config
        .modeBarButtonsToRemove(Seq(
          "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d",
          "hoverClosestCartesian", "hoverCompareCartesian",
          "toggleSpikelines",
        ).toJSArray)
        ._result
    )
    //

    val extraTraceManager = new ExtraTraceManager(plotDiv, dataSeq.size)
    var snowflakeRef: Option[ExtraTracesRef] = None
    var selectedRichPoint: Option[RichPoint] = None
    var ticksRef: Option[ExtraTracesRef] = None
    var improvementsRef: Option[ExtraTracesRef] = None
    var componentSumRef: Option[ExtraTracesRef] = None

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

      /*
      plotDataSeq = plotDataSeq :+ {
        scatter
          .x(js.Array(rp.point(0)))
          .y(js.Array(rp.point(1)))
          .marker(marker
            .symbol(asterisk)
            .color(0 at 3)
          )
          ._result
      }
      */

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
              //.width(1)
              .dash("dot")
              .color(/*(0.5 at 3)*/ colors(i)/*.opacity(0.5)*/)
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
              .color(colors(i)/*.opacity(1.0)*/)
            )
            .hoverinfo("skip")
            ._result
        })
      }

      val referenceColor = 0.5 at 3

      def ticksDataSeq(rp: RichPoint) = {
        (0 until dimension).flatMap(i => {
          val cartesianComponentVector = basis.component(rp.plotOutput, i)
          val polarComponentVector = polarFromCartesian(cartesianComponentVector)
          if(polarComponentVector.radius == 0) {
            None
          } else {
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
              .line(customLine
                .shape("spline")
                //.color(colors(i))
              )
              //.fill("toself")
              //.fillcolor(/*colors(i)*/referenceColor.toOMColor.toJS.toString)
              //.fillcolor(colors(i).toOMColor.toJS.toString)
              .hoverinfo("skip")
              ._result
            )
          }
        })
      }

      def improvementsDataSeq(rp: RichPoint) = {
        val improvementParetoFront = pointPlotter.plotOutputs.map { v =>
          (v - rp.plotOutput).map(pointPlotter.betterPlot match {
            case BetterPlot.IsLower => _ < 0
            case BetterPlot.IsHigher => _ > 0
          })
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
              val n = 4
              val p = 1
              val points = (p to n - p).map(_.toDouble / n).map { alpha =>
                cartesianFromPolar(Seq(1.3 * rp.radius, polarFromCartesian((1 - alpha) * p1 + alpha * p2).angle))
              }.map(_.add(rp.point))
              val coordinates = points.transpose
              Some(scatter
                .x(coordinates(0).toJSArray)
                .y(coordinates(1).toJSArray)
                .setMode(lines)
                .line(line
                  .width(4)
                  .color(colors(i)/*.opacity(1.0)*/)
                  .shape("spline")
                )
                .hoverinfo("skip")
                ._result)
            } else {
              None
            }
          }

          /*
          improvementVector.zipWithIndex.filter(_._1).map(_._2).map { i =>
            val petalLength = style match {
              case Polygon => rp.radius/2
              case Flower => rp.radius
            }
            val angle = polarFromCartesian(basis.basisVector(i)).angle
            val angleDelta = 360.0/dimension/2

            lazy val trapezoid = {
              val quad = Seq(rp.radius, rp.radius + petalLength).map { r =>
                Seq(angle - angleDelta, angle + angleDelta).map { a =>
                  rp.point.add(cartesianFromPolar(Seq(r, a)))
                }
              }
              val rectangularQuad = Seq(quad(0)(0), quad(1)(0), quad(1)(1), quad(0)(1))
              val closedLine = rectangularQuad :+ rectangularQuad.head
              closedLine
            }

            lazy val petal = {
              val alpha = 2
              val n = 8
              val halfPetal = (0 until n).map(_.toDouble / n)
                .map(x => Seq(pow(1 - pow(1 - x, alpha), alpha), x))
                .map(_.add(Seq(0.0, -1.0)))
              val wholePetal = halfPetal ++ halfPetal.map(_.mul(Seq(1, -1))).reverse
              val overlap = rp.radius/2 //experimental feature
              val scaledPetal = wholePetal.map(_
                .mul(Seq(overlap + petalLength, angleDelta))
                .add(Seq(0, angle))
              )
              val placedPetal = scaledPetal
                .map(cartesianFromPolar)
                .map(_
                  .add(basis.basisVector(i).scale(rp.radius - overlap))
                  //.scale((rp.radius + petalLength)/(overlap + petalLength))
                  .add(rp.point)
                )
                //.filter(_.distance(rp.point) > rp.radius)
              val closedPetal = placedPetal :+ placedPetal.head
              closedPetal
            }

            val points = style match {
              case Polygon => trapezoid
              case Flower => petal
            }
            val coordinates = points.transpose
            scatter
              .x(coordinates(0).toJSArray)
              .y(coordinates(1).toJSArray)
              .setMode(lines)
              .line(style match {
                case Polygon => customLine
                  .width(1)
                case Flower => customLine
                  .width(1)
                  .shape("spline")
              })
              .fill("toself")
              .fillcolor(colors(i).toOMColor.toJS.toString)
              .hoverinfo("skip")
              ._result

            /*
            val points = Seq(rp.point, rp.point + basis.basisVector(i).scale(0.1))
            val coordinates = points.transpose
            scatter
              .x(coordinates(0).toJSArray)
              .y(coordinates(1).toJSArray)
              .setMode(lines)
              .marker(marker
                .color(colors(i))
              )
              .hoverinfo("skip")
              ._result
            */
          }
          */

          /*
          val improvement = count.toDouble/dimension
          val coordinates = Seq(rp.point).transpose

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
                  .size(rp.size + 2 * /*customLine.width*/2 + improvement * size)
                  .symbol(circle.open)
                  .set(line
                    .width(2)
                  ) //TODO .line(
                  //.color(customLine.color)
              }
            )
            .hoverinfo("skip")
            ._result
          )
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
                .size(rp.size + d * size)
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

          /*
          (1 to count).map { d =>
            val p = d.toDouble/dimension
            scatter
              .x(coordinates(0).toJSArray)
              .y(coordinates(1).toJSArray)
              .marker(marker
                .size(rp.size + p * size)
                .symbol(circle)
                .color(0.7 at 3)
                .opacity(1 - p)
              )
              .hoverinfo("skip")
              ._result
          }.reverse
          */

          /*
          Seq(
            scatter
              .x(coordinates(0).toJSArray)
              .y(coordinates(1).toJSArray)
              .marker(marker
                .size(rp.size + improvement * size)
                .symbol(circle)
                .color(rp.color)
                .set(line.
                  width(1)
                  .color(0.5 at 3)
                )
              )
              .hoverinfo("skip")
              ._result
          )
          */
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
        case _ => improvementsRef = extraTraceManager.updateTraces(improvementsRef, selectedRichPoint.map(improvementsDataSeq))
      }

      snowflake match {
        case Add => snowflakeRef = extraTraceManager.updateTraces(snowflakeRef, Some(snowflakeDataSeq))
        case Remove => snowflakeRef = extraTraceManager.updateTraces(snowflakeRef, None)
        case _ =>
      }

      updateImprovements match {
        case NoAction =>
        case _ => ticksRef = extraTraceManager.updateTraces(ticksRef, selectedRichPoint.map(ticksDataSeq))
      }

      componentSum match {
        case Add => componentSumRef = extraTraceManager.updateTraces(componentSumRef, Some(componentSumDataSeq))
        case Remove => componentSumRef = extraTraceManager.updateTraces(componentSumRef, None)
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
    plotDiv.ref.on("plotly_relayout", _ => skipOnBusy.skipOnBusy("relayout", () => {
      extraTraceManager.deleteAllTraces()
      snowflakeRef = None
      selectedRichPoint = None
      ticksRef = None
      improvementsRef = None
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
