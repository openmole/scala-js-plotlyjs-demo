package plotlyjs.demo

import org.openmole.plotlyjs._
import scaladget.bootstrapnative.bsn._
import scaladget.tools._
import scalatags.JsDom.svgAttrs.{height, style, width, x, y}
import org.scalajs.dom.raw.{Element, MouseEvent}
import plotlyjs.demo.Data.{DimensionName, PSEData, pse}
import scalatags.JsDom.all._
import scalatags.JsDom.svgTags
import rx._

import scala.annotation.tailrec


object PSESVGDemo {
  val sc = sourcecode.Text {

    val gridSize = 800

    val scene = svgTags.svg(
      width := gridSize + 50,
      height := gridSize + 50
    ).render


    import data._

    case class Dimensions(x: DimensionName, y: DimensionName, z: DimensionName)
    lazy val dimensions = Var(Dimensions(Data.area, Data.compacity, Data.convexity))

    case class Coordinate(cx: Int, cy: Int, cz: Int)
    def findCordinate(coords: Seq[Coordinate], cx: Int, cy: Int) = coords.find(c => c.cx == cx && c.cy == cy)


    def colorPaletteRatio(cz: Int, zSize: Int) = {
      val oo = (cz * plotlyjs.demo.Utils.viridis.size.toDouble / zSize).toInt - 1
      //  println("CZ " + cz + " // " + oo)
      oo
    }


    val pseScene = div(
      Rx {

        println(s"X ${dimensions.now.x}, Y ${dimensions.now.y}, Z ${dimensions.now.z}")

        @tailrec
        def getDataCells0(x: PSEData, y: PSEData, z: PSEData, coordinates: Seq[Coordinate]): Seq[Coordinate] = {
          if (x.values.isEmpty) coordinates
          else {
            val cx = x.dimension.sampling.search(x.values.head).insertionPoint
            val cy = y.dimension.sampling.search(y.values.head).insertionPoint
            val cz = z.dimension.sampling.search(z.values.head).insertionPoint
           // println(s"${x.values.head} / ${y.values.head} / ${z.values.head} => $cx / $cy / $cz")
            val existingCoordinate = findCordinate(coordinates, cx, cy)
            //  println("SEARCH: " + data._3.head + " __ " + cSequence.search(data._3.head).insertionPoint)
            getDataCells0(
              x.copy(values = x.values.tail), y.copy(values = y.values.tail), z.copy(values = z.values.tail),
              coordinates :+ existingCoordinate.getOrElse(Coordinate(cx, cy, 0)).copy(cz = cz + 1)
            )
          }
          //  Utils.viridis(cSequence.search(data._3.head).insertionPoint))
        }

        val psex = Data.psedata(Data.pse, dimensions().x)
        val psey = Data.psedata(Data.pse, dimensions().y)
        val psez = Data.psedata(Data.pse, dimensions().z)

        val dataCells = getDataCells0(psex, psey, psez, Seq())

        println("DATACELLS " + dataCells)

        val xCellSize = gridSize.toDouble / psex.dimension.size
        val padding = xCellSize * 3 / 4
        val yCellSize = gridSize.toDouble / psey.dimension.size

        val cellWidth = gridSize / psex.dimension.size
        val cellHeight = gridSize / psey.dimension.size

        for {
          c <- dataCells
        } yield {
          scene.appendChild(
            svgTags.rect(x := c.cx * xCellSize, y := (psey.dimension.size - c.cy) * yCellSize - 50, width := cellWidth, height := cellHeight,
              style := s"fill:#${plotlyjs.demo.Utils.viridis(colorPaletteRatio(c.cz, psez.dimension.size))};", onmouseover := { () =>
                println(s"X ${c.cx} Y ${c.cy} Z ${c.cz}")
              }
            ).render
          )
        }

        for (
          (value, id) <- psex.dimension.sampling.zipWithIndex
        ) yield {
          scene.appendChild(
            svgTags.text(x := id * xCellSize + padding, y := gridSize + 50, minWidth := xCellSize, value).render
          )
        }

        // println("DATACELLS " + dataCells)
        scene.render

      }
    )


    //  val pseScene = Rx {


    // }


    def switch(pressed: DimensionName) = {
      val newDimensions = Seq(dimensions.now.x, dimensions.now.y, dimensions.now.z).filterNot {
        _ == pressed
      } :+ pressed
      dimensions() = Dimensions(newDimensions(0), newDimensions(1), newDimensions(2))
    }

    val xButton = button(btn_default, Data.area, onclick := { () => switch(Data.area) })
    val yButton = button(btn_default, Data.compacity, onclick := { () => switch(Data.compacity) })
    val zButton = button(btn_default, Data.convexity, onclick := { () => switch(Data.convexity) })


    div(
      xButton, yButton, zButton,
      pseScene
    ).render

  }

  val elementDemo = new ElementDemo {
    def title: String = "PSE SVG"

    def code: String = sc.source

    def element: Element = sc.value
  }

}
