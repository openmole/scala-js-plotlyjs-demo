package plotlyjs.demo.homemade.pareto

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.openmole.plotlyjs.HoverMode.closest
import org.openmole.plotlyjs.PlotMode.{lines, markers}
import org.openmole.plotlyjs.PlotlyImplicits.elToPlotlyElement
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all.{PlotMarkerAPI, _}
import org.scalajs.dom.html
import plotlyjs.demo.homemade.api.Data.Outcome
import plotlyjs.demo.homemade.api.Pareto.{Maximization, ParetoDisplay, ParetoObjective}
import plotlyjs.demo.homemade.pareto.PointPlotter.BetterPlot
import plotlyjs.demo.utils.Colors._
import plotlyjs.demo.utils.{Colors, ParetoFront}
import plotlyjs.demo.utils.Utils.{ExtraTraceManager, SkipOnBusy}
import plotlyjs.demo.utils.vector.Vectors._

import scala.math.Numeric.BigDecimalAsIfIntegral.abs
import scala.math.ceil
import scala.scalajs.js
import scala.scalajs.js.JSConverters.JSRichIterableOnce
import scala.scalajs.js.Object.entries

object Pareto { //TODO no zoom, no useless button

  def plot(objectives: Seq[ParetoObjective], paretoFront: Seq[Outcome], paretoDisplay: ParetoDisplay): ReactiveHtmlElement[html.Div] = {
    val dimension = objectives.size
    val downHalfDimension = dimension / 2
    val upHalfDimension = ceil(dimension / 2.0).toInt

    val basis = new SnowflakeBasis(dimension)

    val spaceNormalObjectives = (0 until dimension).map((0 at dimension).replace(_, 1))
    val cartesianObjectives = spaceNormalObjectives.map(basis.transform)
    val polarObjectives = cartesianObjectives.map(SnowflakeBasis.polarFromCartesian)
    val colors = polarObjectives.map(vector => Seq(((vector(1) + 360) % 360) / 360, 1, 0.5).fromHSLtoRGB.opacity(0.5))
    val axisShapeSeq = cartesianObjectives.zipWithIndex.map { case (o, i) =>
      Shape
        .`type`("line")
        .x0(0).y0(0)
        .x1(o(0)).y1(o(1))
        .line(line
          .color(colors(i))
        )
        ._result
    }
    val legendAnnotationSeq = cartesianObjectives.zipWithIndex.map { case (o, i) =>
      val textPosition = 1.1 * o
      Annotation
        .x(textPosition(0))
        .y(textPosition(1))
        .text((if(objectives(i).optimizationType == Maximization) "-" else "") + objectives(i).name)
        .showarrow(false)
        ._result
    }

    val pointPlotter = PointPlotter(
      objectives.map(_.optimizationType),
      paretoFront.map(_.outputs.map(_.value)),
      if(paretoDisplay.lowerIsBetter) BetterPlot.IsLower else BetterPlot.IsHigher
    )

    case class RichPoint(x: Double, y: Double, index: Int, color: Colors.Color)
    val richPoints = pointPlotter
      .plotOutputs
      .map(basis.transform)
      .zipWithIndex.map { case (point, index) => RichPoint(point(0), point(1), index, pointPlotter.betterPlot match {
      case BetterPlot.IsLower => colors(pointPlotter.plotOutputs(index).zipWithIndex.minBy(_._1)._2)
      case BetterPlot.IsHigher => colors(pointPlotter.plotOutputs(index).zipWithIndex.maxBy(_._1)._2)
    })}

    val markerSize = 8
    val paretoFrontDataSeq = richPoints.map { p =>
      scatter
        .x(Seq(p.x).toJSArray)
        .y(Seq(p.y).toJSArray)
        .setMode(markers)
        .marker(marker
          .size(markerSize)
          .symbol(circle)
          .color(p.color)
        )
        .hoverinfo("text")
        .text("click me") //or changes color on hover ?
        .customdata(Seq(p.index.toString).toJSArray)
        ._result
    }

    val borderShape = {

      val radius = {
        //basis.transform((1 at upHalfDimension) ++ (0 at dimension - upHalfDimension)).norm //Leave space for the componentSum to display...
        richPoints.map(p => Seq(p.x, p.y).norm).max //... or fit the points.
      }

      Shape
        .`type`("circle")
        .xref("x")
        .yref("y")
        .x0(-radius)
        .y0(-radius)
        .x1(radius)
        .y1(radius)
        .line(line
          .width(1)
          .color(0.0 at 4)
        )
        ._result
    }

    //Display
    val plotDiv = div()
    val dataSeq = Seq()
    val shapeSeq = axisShapeSeq ++ (if (dimension == 2) None else Some(borderShape))
    val annotationSeq = legendAnnotationSeq
    //val size = 800
    Plotly.newPlot(
      plotDiv.ref,
      dataSeq.toJSArray,
      Layout
        .title("Pareto")
        //.height(size)
        //.width(size)
        .xaxis(axis
          .visible(false)
        )
        .yaxis(axis
          .scaleanchor("x")
          .visible(false)
        )
        .showlegend(false)
        //.asJsOpt("paper_bgcolor", Color.rgb(245, 245, 245).toJS)
        .paperbgcolor(245.toDouble / 256 at 3) //TODO not working
        .shapes(shapeSeq.toJSArray)
        .annotations(annotationSeq.toJSArray)
        .hovermode(closest)
        ._result
    )
    //

    //Events and paretoFrontData
    def get[A](plotData: PlotData, key: String, index: Int): Option[A] = entries(plotData).filter(_._1 == key).headOption.map(_._2.asInstanceOf[scala.scalajs.js.Array[A]](index))

    val extraTraceManager = new ExtraTraceManager(plotDiv, dataSeq.size)
    var currentParetoFrontDisplay = paretoFrontDataSeq
    extraTraceManager.addTraces(currentParetoFrontDisplay)
    var currentSnowflakeReference = Seq[PlotData]()

    val rawOutputCoordinates = Var(div(""))

    def eventHandler(pointsData: PointsData,
                     focusedPoint: Boolean = false,
                     coordinateSnowflake: Boolean = false,
                     componentSum: Boolean = false,
                     compromiseHelp: Boolean = false
                    ): Unit = {
      val pointData = pointsData.points.head

      get[String](pointData.data, "customdata", pointData.pointNumber).map(_.toInt).foreach(index => {
        val indexedPolarPoint = richPoints(index)
        val plotOutput = pointPlotter.plotOutputs(index)

        var cartesianEnd = 0.0 at 2

        lazy val _focusedPoint = {
          Seq(
            scatter
              .x(js.Array(indexedPolarPoint.x))
              .y(js.Array(indexedPolarPoint.y))
              .marker(marker
                .size(markerSize + 4)
                .symbol(circle.open)
                .color(0.5 at 3)
              )
              ._result
          )
        }
        lazy val snowflake = {
          (0 until dimension).map(i => {
            val componentVector = basis.component(plotOutput, i)
            val coordinates = Seq(0.0 at 2, componentVector).transpose
            scatter
              .x(coordinates(0).toJSArray)
              .y(coordinates(1).toJSArray)
              .setMode(lines)
              .line(line
                .width(8)
                .color(colors(i))
              )
              .hoverinfo("none")
              ._result
          })
        }
        lazy val snowflakeReference = {
          (0 until dimension).map(i => {
            val componentVector = basis.component(plotOutput, i)
            val (p1, p2) = {
              val angle = SnowflakeBasis.polarFromCartesian(componentVector).angle
              val radius = 0.05
              (
                componentVector.add(SnowflakeBasis.cartesianFromPolar(Seq(radius, angle + 90))),
                componentVector.add(SnowflakeBasis.cartesianFromPolar(Seq(radius, angle - 90)))
              )
            }
            val coordinates = Seq(p1, p2).transpose
            scatter
              .x(coordinates(0).toJSArray)
              .y(coordinates(1).toJSArray)
              .setMode(lines)
              .line(line
                .color(colors(i))
              )
              .hoverinfo("none")
              ._result
          })
        }
        lazy val _componentSum = {
          plotOutput.zipWithIndex.sortBy({ case (c, _) => abs(c) }).reverse.map(_._2).map(i => {
            val cartesianComponentVector = basis.component(plotOutput, i)
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
              .hoverinfo("none")
              ._result
          })
        }
        lazy val oneObjectiveCompromise = { //very few compromises in high dimensions
          val neighbourhood = ParetoFront.oneObjectiveCompromiseGraph(pointPlotter.plotOutputs, plotOutput)
          neighbourhood.arrows.flatMap(arrow => {
            val coordinates = Seq(arrow.tail, arrow.head).map(basis.transform).transpose
            Seq(
              scatter
                .x(coordinates(0).toJSArray)
                .y(coordinates(1).toJSArray)
                .setMode(lines)
                .line(line
                  .width(1)
                  .color(colors(arrow.weight).opacity(1.0))
                )
                .hoverinfo("none")
                ._result
            )
          })
        }
        lazy val multiObjectiveCompromise = {
          val compromiseParetoFront = pointPlotter.plotOutputs.map { v =>
            (v, (v - plotOutput).count(pointPlotter.betterPlot match {
              case BetterPlot.IsLower => _ > 0
              case BetterPlot.IsHigher => _ < 0
            }))
          }
          val minSize = markerSize
          val maxSize = 4 * markerSize
          val (worstSize, bestSize) = pointPlotter.betterPlot match {
            case BetterPlot.IsLower => (maxSize, minSize)
            case BetterPlot.IsHigher => (minSize, maxSize)
          }
          compromiseParetoFront.zipWithIndex.map { case ((point, count), index) => {
            val compromise = count.toDouble/dimension //TODO dimension - 1 ?
            val coordinates = Seq(point).map(basis.transform).transpose
            scatter
              .x(coordinates(0).toJSArray)
              .y(coordinates(1).toJSArray)
              .marker(marker
                .size(if(count == 0) markerSize else compromise * worstSize + (1 - compromise) * bestSize)
                .symbol(if(count == 0) circle.open else circle)
                .set(if(count == 0) line
                  .width(2)
                else line
                  .width(0)
                ) //TODO .line(
                .color(richPoints(index).color)
              )
              .customdata(Seq(index.toString).toJSArray)
              .hoverinfo("text")
              .text("click me")
              ._result
            }
          }
        }
        var plotDataSeq = Seq[PlotData]()//componentSum ++ multiObjectiveCompromise ++ coordinateStar //++ oneObjectiveCompromise
        if(componentSum) plotDataSeq = plotDataSeq ++ _componentSum
        if(compromiseHelp) {
          currentParetoFrontDisplay = multiObjectiveCompromise
          currentSnowflakeReference = snowflakeReference
        }
        if(focusedPoint) plotDataSeq = plotDataSeq ++ _focusedPoint
        if(coordinateSnowflake) plotDataSeq = plotDataSeq ++ snowflake

        extraTraceManager.deleteTraces()
        extraTraceManager.addTraces(currentParetoFrontDisplay)
        extraTraceManager.addTraces(plotDataSeq)
        extraTraceManager.addTraces(currentSnowflakeReference)

        val textDiv = div()
        textDiv.ref.innerHTML = "Model output :<br>" + (pointPlotter.outputs(index).zipWithIndex map { case (c, i) => s"o${i + 1} : $c" }).mkString("<br>")
        rawOutputCoordinates.set(textDiv)
      })
    }

    val skipOnBusy = new SkipOnBusy
    plotDiv.ref.on("plotly_hover", pointsData => skipOnBusy.skipOnBusy(eventHandler(pointsData, coordinateSnowflake = true, componentSum = paretoDisplay.outputPath)))
    plotDiv.ref.on("plotly_click", pointsData => skipOnBusy.skipOnBusy(eventHandler(pointsData, compromiseHelp = true)))
    plotDiv.ref.on("plotly_relayout", _ => skipOnBusy.skipOnBusy({
      extraTraceManager.deleteTraces()
      currentParetoFrontDisplay = paretoFrontDataSeq
      extraTraceManager.addTraces(currentParetoFrontDisplay)
      currentSnowflakeReference = Seq()
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
