package plotlyjs.demo.labo

import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.HoverMode.closest
import org.openmole.plotlyjs.PlotMode.{lines, markers}
import org.openmole.plotlyjs.PlotlyImplicits.elToPlotlyElement
import org.openmole.plotlyjs.all.{axis, circle, line, marker, scatter}
import org.openmole.plotlyjs._
import plotlyjs.demo.demo.Demo
import plotlyjs.demo.utils.PointSet.MIN
import plotlyjs.demo.utils.Utils.{ExtraTraceManager, SkipOnBusy}
import plotlyjs.demo.utils.vector.Vectors._
import plotlyjs.demo.utils.Colors._
import plotlyjs.demo.utils.{Basis, ParetoFront, PointSet, Utils}

import scala.math.Numeric.BigDecimalAsIfIntegral.abs
import scala.math.{atan2, ceil, cos, sin}
import scala.scalajs.js
import scala.scalajs.js.JSConverters.JSRichIterableOnce
import scala.scalajs.js.Object.entries

object ParetoBisDemo {

  private lazy val sc = sourcecode.Text {

    def polarFromCartesian(vector: Vector): Vector = {
      val r = vector.norm
      val x = vector(0)
      val y = vector(1)
      val theta = atan2(y, x).toDegrees
      Seq(r, theta)
    }

    def cartesianFromPolar(vector: Vector): Vector = {
      val r = vector(0)
      val theta = vector(1).toRadians
      val x = r * cos(theta)
      val y = r * sin(theta)
      Seq(x, y)
    }

    class StarBasis(dimension: Int) extends Basis {

      override val size: Int = dimension

      override def basisVector(i: Int): Vector = {
        if (dimension == 2) {
          i match {
            case 0 => cartesianFromPolar(Seq(1, 0))
            case 1 => cartesianFromPolar(Seq(1, 90))
          }
        } else {
          cartesianFromPolar(Seq(1, 360 * i / dimension))
        }
      }

    }

    def paretoFrontDiv(paretoFrontPoints: Seq[Vector]) = {
      val dimension = paretoFrontPoints.head.dimension
      val downHalfDimension = dimension / 2
      val upHalfDimension = ceil(dimension / 2.0).toInt

      val basis = new StarBasis(dimension)

      val spaceNormalObjectives = (0 until dimension).map((0 at dimension).replace(_, 1))
      val cartesianObjectives = spaceNormalObjectives.map(basis.transform)
      val polarObjectives = cartesianObjectives.map(polarFromCartesian)
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
          .text("o" + (i + 1))
          .showarrow(false)
          ._result
      }

      val pointSet = new PointSet(paretoFrontPoints)
        .optimizationProblems(MIN at dimension) //To configure with metadata.
        .lowerPlotIsBetter //Reverses MAX dimensions.

      case class IndexedPoint(x: Double, y: Double, index: Int)
      val points = pointSet
        .spaceNormalizedOutputs
        .map(basis.transform)
        .zipWithIndex.map { case (point, index) => IndexedPoint(point(0), point(1), index) }

      val markerSize = 8
      val pointColor = 0.5 at 4
      val paretoFrontData = scatter
        .x(points.map(_.x).toJSArray)
        .y(points.map(_.y).toJSArray)
        .setMode(markers)
        .set(marker //TODO .marker(
          .size(markerSize)
          .set(circle) //TODO .marker(
          .set(pointColor) //TODO .color(
        )
        .hoverinfo("text")
        .text("click me")
        .customdata(points.map(_.index.toString).toJSArray)
        ._result

      val borderShape = {

        val radius = {
          //basis.transform((1 at upHalfDimension) ++ (0 at dimension - upHalfDimension)).norm //Leave space for the componentSum to display...
          points.map(p => Seq(p.x, p.y).norm).max //... or fit the points.
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
      val dataSeq = Seq() ///*objectivesDataSeq :+ */Seq(paretoFrontData)
      val shapeSeq = axisShapeSeq ++ (if (dimension == 2) None else Some(borderShape))
      val annotationSeq = legendAnnotationSeq
      val size = 800
      Plotly.newPlot(
        plotDiv.ref,
        dataSeq.toJSArray,
        Layout
          .title("Pareto")
          .height(size)
          .width(size)
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

      //Events and paretoFrontData
      def get[A](plotData: PlotData, key: String, index: Int): Option[A] = entries(plotData).filter(_._1 == key).headOption.map(_._2.asInstanceOf[scala.scalajs.js.Array[A]](index))

      val defaultTraces = Seq(paretoFrontData)
      val extraTraceManager = new ExtraTraceManager(plotDiv, dataSeq.size)
      extraTraceManager.addTraces(defaultTraces)

      val rawOutputCoordinates = Var(div(""))

      def eventHandler(pointsData: PointsData): Unit = {
        println("eventHandler called")
        val pointData = pointsData.points.head

        get[String](pointData.data, "customdata", pointData.pointNumber).map(_.toInt).foreach(index => {
          val indexedPolarPoint = points(index)
          val plotOutput = pointSet.spaceNormalizedOutputs(index)

          var cartesianEnd = 0.0 at 2

          lazy val focusedPoint = {
            Seq(
              scatter
                .x(js.Array(indexedPolarPoint.x))
                .y(js.Array(indexedPolarPoint.y))
                .set(marker //TODO .marker(
                  .size(markerSize + 4)
                  .set(circle.open) //TODO .symbol(
                  .set(0.5 at 3) //TODO .color(
                )
                ._result
            )
          }
          lazy val coordinateStar = {
            (0 until dimension).map(i => {
              val cartesianComponentVector = basis.component(plotOutput, i)
              val starPolarCoordinates = Seq(0.0 at 2, cartesianComponentVector).transpose
              scatter
                .x(starPolarCoordinates(0).toJSArray)
                .y(starPolarCoordinates(1).toJSArray)
                .setMode(lines)
                .line(line
                  .width(8)
                  .color(colors(i))
                )
                /*
                .marker(marker
                  .size(0)
                )
                */
                .hoverinfo("none")
                ._result
            })
          }
          lazy val componentSum = {
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
          lazy val oneObjectiveCompromise = { //very few compromises in high dimension
            val neighbourhood = ParetoFront.oneObjectiveCompromiseGraph(pointSet.spaceNormalizedOutputs, plotOutput)
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
            val neighbourhood = ParetoFront.compromise(pointSet.spaceNormalizedOutputs, plotOutput)
            neighbourhood.zipWithIndex.map { case ((point, count), index) => {
              val coordinates = Seq(point).map(basis.transform).transpose
              scatter
                .x(coordinates(0).toJSArray)
                .y(coordinates(1).toJSArray)
                .set(marker //TODO .marker(
                  .size(markerSize + 4 * count)
                  .set(circle) //TODO .symbol(
                  .set(pointColor) //TODO .color(
                )
                .customdata(Seq(index.toString).toJSArray)
                .hoverinfo("none")
                ._result
            }
            }
          }
          val plotDataSeq = /*componentSum ++ multiObjectiveCompromise ++*/ coordinateStar //++ oneObjectiveCompromise

          extraTraceManager.deleteTraces()
          extraTraceManager.addTraces(plotDataSeq)

          val textDiv = div()
          textDiv.ref.innerHTML = "Model output :<br>" + (pointSet.rawOutputs(index).zipWithIndex map { case (c, i) => s"o${i + 1} : $c" }).mkString("<br>")
          rawOutputCoordinates.set(textDiv)
        })
      }

      val skipOnBusy = new SkipOnBusy
      plotDiv.ref.on("plotly_click", pointsData => skipOnBusy.skipOnBusy(eventHandler(pointsData)))
      plotDiv.ref.on("plotly_relayout", _ => skipOnBusy.skipOnBusy({
        extraTraceManager.deleteTraces()
        extraTraceManager.addTraces(defaultTraces)
      }))
      //

      div(
        plotDiv,
        div(child <-- rawOutputCoordinates.signal)
      )
    }

    div(
      paretoFrontDiv(Utils.randomizeDimensions(ParetoFront.random(2, 42))),
      paretoFrontDiv(Utils.randomizeDimensions(ParetoFront.random(3, 42))),
      paretoFrontDiv(Utils.randomizeDimensions(ParetoFront.random(4, 42))),
      paretoFrontDiv(Utils.randomizeDimensions(ParetoFront.random(5, 42))),
      paretoFrontDiv(Seq(
        Seq(1.0, 1.0, 0.0, 0.0),
        Seq(0.0, 1.0, 1.0, 0.0),
        Seq(0.0, 0.0, 1.0, 1.0),
        Seq(1.0, 0.0, 0.0, 1.0),

        Seq(1.1, 0.1, 1.0, 0.0),
        Seq(0.0, 1.0, 0.1, 1.1),
      )),
      paretoFrontDiv(Seq(
        Seq(1.0, 0.0, 0.0, 0.0, 0.0, 0.0),
        Seq(0.0, 1.0, 0.0, 0.0, 0.0, 0.0),
        Seq(0.0, 0.0, 1.0, 0.0, 0.0, 0.0),
        Seq(0.0, 0.0, 0.0, 1.0, 0.0, 0.0),
        Seq(0.0, 0.0, 0.0, 0.0, 1.0, 0.0),
        Seq(0.0, 0.0, 0.0, 0.0, 0.0, 1.0),

        Seq(1.0, 1.0, 1.0, 0.1, 0.1, 0.1),
      )),
      paretoFrontDiv(Seq(
        Seq(1.0, 0.0, 0.0, 0.0, 0.0, 0.0),
        Seq(0.0, 1.0, 0.0, 0.0, 0.0, 0.0),
        Seq(0.0, 0.0, 1.0, 0.0, 0.0, 0.0),
        Seq(0.0, 0.0, 0.0, 1.0, 0.0, 0.0),
        Seq(0.0, 0.0, 0.0, 0.0, 1.0, 0.0),
        Seq(0.0, 0.0, 0.0, 0.0, 0.0, 1.0),

        Seq(1.0, 0.1, 1.0, 0.1, 1.0, 0.1),
      ))
    )
  }

  val elementDemo: Demo = new Demo {
    def title: String = "Pareto bis"

    def code: String = sc.source

    def element: HtmlElement = sc.value
  }

}
