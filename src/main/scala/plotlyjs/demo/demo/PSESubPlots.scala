package plotlyjs.demo.demo

import org.scalajs.dom.raw.{Element, MouseEvent}
import org.openmole.plotlyjs.PlotlyImplicits._
import com.raquo.laminar.api.L._

import scala.annotation.tailrec
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import plotlyjs.demo.utils.Data
import plotlyjs.demo.utils.Data._

import scala.scalajs.js.JSConverters._

object PSESubPlots {
  val sc = sourcecode.Text {

    val plotDiv = div()


    case class Dimensions(x: DimensionName, y: DimensionName, z: DimensionName)
    lazy val dimensions = Dimensions(Data.area, Data.compacity, Data.convexity)

    case class Value(x: Double, y: Double)


    def getPoints = {

      @tailrec
      def getDataCells0(x: PSEData, y: PSEData, z: PSEData, coordinates: Map[Int, Seq[Value]]): Map[Int, Seq[Value]] = {
        if (x.values.isEmpty) coordinates
        else {
          val cz = z.dimension.sampling.search(z.values.head).insertionPoint
          getDataCells0(
            x.copy(values = x.values.tail), y.copy(values = y.values.tail), z.copy(values = z.values.tail),
            coordinates + ((cz -> (coordinates.getOrElse(cz, Seq()) :+ Value(x.values.head, y.values.head))))
          )
        }
        //  Utils.viridis(cSequence.search(data._3.head).insertionPoint))
      }

      val psex = Data.psedata(Data.pse, dimensions.x)
      val psey = Data.psedata(Data.pse, dimensions.y)
      val psez = Data.psedata(Data.pse, dimensions.z)

      getDataCells0(psex, psey, psez, Map())

    }

    val layout = Layout
      .title("My line plot")
      .grid(grid.columns(1).rows(10).pattern(Pattern.coupled).rowOrder(RowOrder.bottomToTop).ygap(20))
      .height(800)
      .showlegend(false)
      .xaxis(Axis.zeroline(false))
      .xaxis2(Axis.visible(false))
      .xaxis3(Axis.visible(false))
      .xaxis4(Axis.visible(false))
      .xaxis5(Axis.visible(false))
      .xaxis6(Axis.visible(false))
      .xaxis7(Axis.visible(false))
      .xaxis8(Axis.visible(false))
      .xaxis9(Axis.visible(false))
      .xaxis10(Axis.visible(false))
//      .xaxis3(Axis.visible(false).tickmode(TickMode.linear).dtick(1000))
//      .xaxis4(Axis.visible(false).tickmode(TickMode.linear).dtick(1000))
//      .xaxis5(Axis.visible(false).tickmode(TickMode.linear).dtick(1000))
//      .xaxis6(Axis.visible(false).tickmode(TickMode.linear).dtick(1000))
//      .xaxis7(Axis.visible(false).tickmode(TickMode.linear).dtick(1000))
//      .xaxis8(Axis.visible(false).tickmode(TickMode.linear).dtick(1000))
//      .xaxis9(Axis.visible(false).zeroline(false).tickmode(TickMode.linear).dtick(1000))
//      .xaxis10(Axis.visible(false).zeroline(false).tickmode(TickMode.linear).dtick(1000))
      .yaxis(Axis.dtick(0.5))
      .yaxis2(Axis.dtick(0.5).zeroline(false))
      .yaxis3(Axis.zeroline(false).dtick(0.5))
      .yaxis4(Axis.zeroline(false).dtick(0.5))
      .yaxis5(Axis.zeroline(false).dtick(0.5))
      .yaxis6(Axis.zeroline(false).dtick(0.5))
      .yaxis7(Axis.zeroline(false).dtick(0.5))
      .yaxis8(Axis.zeroline(false).dtick(0.5))
      .yaxis9(Axis.zeroline(false).dtick(0.5))
      .yaxis10(Axis.zeroline(false).dtick(0.5))

    val points = getPoints
    val keys = points.keys.toSeq.sorted

//    val data0 = scatter
//        .x(points(keys.head).map{_.x}.toJSArray)
//        .y(points(keys.head).map{_.y}.toJSArray)._result

    val dataSeq = (for {
      (z, ind) <- keys.zipWithIndex
    } yield {
      println("G: " + s"x${ind + 1 }")
      scatter
        .x(points(z).map{_.x}.toJSArray)
        .y(points(z).map{_.y}.toJSArray).xaxis(s"x${ind + 1 }").yaxis(s"y${ind + 1 }")._result
    })

    Plotly.newPlot(plotDiv.ref, dataSeq.toJSArray, layout = layout)
    plotDiv

  }

  val elementDemo = new ElementDemo {
    def title: String = "PSE Subplots"

    def code: String = sc.source

    def element: HtmlElement = sc.value
  }

}
