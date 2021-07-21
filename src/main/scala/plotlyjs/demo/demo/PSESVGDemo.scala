package plotlyjs.demo.demo

import org.openmole.plotlyjs._
import scaladget.bootstrapnative.bsn._
import scaladget.tools._
import org.scalajs.dom.raw.{Element, MouseEvent}
import com.raquo.laminar.api.L._
import com.raquo.laminar.api.L.svg
import plotlyjs.demo.utils.Data
import plotlyjs.demo.utils.Data._
import plotlyjs.demo.utils.Utils

import scala.annotation.tailrec


object PSESVGDemo {
  lazy val sc = sourcecode.Text {

    val gridSize = 800

    case class Dimensions(x: DimensionName, y: DimensionName, z: DimensionName)

    lazy val dimensions = Var(Dimensions(Data.area, Data.compacity, Data.convexity))
    val pseValues = Var("")

    case class Coordinate(cx: Int, cy: Int, cz: Int)

    def findCordinate(coords: Seq[Coordinate], cx: Int, cy: Int) = coords.find(c => c.cx == cx && c.cy == cy)

    def colorPaletteRatio(cz: Int, zSize: Int) = (cz * Utils.viridis.size.toDouble / zSize).toInt - 1


    @tailrec
    def getDataCells0(x: PSEData, y: PSEData, z: PSEData, coordinates: Seq[Coordinate]): Seq[Coordinate] = {
      if (x.values.isEmpty) coordinates
      else {
        val cx = x.dimension.sampling.search(x.values.head).insertionPoint
        val cy = y.dimension.sampling.search(y.values.head).insertionPoint
        val cz = z.dimension.sampling.search(z.values.head).insertionPoint
        val existingCoordinate = findCordinate(coordinates, cx, cy)
        getDataCells0(
          x.copy(values = x.values.tail), y.copy(values = y.values.tail), z.copy(values = z.values.tail),
          coordinates :+ existingCoordinate.getOrElse(Coordinate(cx, cy, 0)).copy(cz = cz + 1)
        )
      }
    }

    div(
      svg.svg(
        svg.width := (gridSize + 50).toString,
        svg.height := (gridSize + 50).toString,
        children <-- dimensions.signal.combineWith(pseValues.signal).map { case (d, t) =>

          val psex = Data.psedata(Data.pse, d.x)
          val psey = Data.psedata(Data.pse, d.y)
          val psez = Data.psedata(Data.pse, d.z)

          val dataCells = getDataCells0(psex, psey, psez, Seq())

          val xCellSize = gridSize.toDouble / psex.dimension.size
          val yCellSize = gridSize.toDouble / psey.dimension.size

          val cellWidth = gridSize / psex.dimension.size
          val cellHeight = gridSize / psey.dimension.size

          Seq(
            Seq(svg.text(t, svg.x := (gridSize / 2).toString, svg.y := "20")),
            for {
              c <- dataCells
            } yield {
              svg.rect(svg.x := (c.cx * xCellSize).toString, svg.y := ((psey.dimension.size - c.cy) * yCellSize - 50).toString, svg.width := cellWidth.toString, svg.height := cellHeight.toString,
                svg.style := s"fill:#${Utils.viridis(colorPaletteRatio(c.cz, psez.dimension.size))};",
                onMouseOver --> { _ =>
                  pseValues.set(s"X ${c.cx} Y ${c.cy} Z ${c.cz}")
                }
              )
            }).flatten
        }
      )
    )

  }

  val elementDemo = new Demo {
    def title: String = "PSE SVG"

    def code: String = sc.source

    def element: HtmlElement = sc.value
  }

}
