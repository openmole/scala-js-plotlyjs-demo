package plotlyjs.demo

import com.raquo.laminar.api.L._

object ParallelCoordinatesDemo {

  private val sc = sourcecode.Text {

    val plotDiv = div()

    plotDiv
  }

  val elementDemo: ElementDemo = new ElementDemo{
    def title: String = "Parallel Coordinates"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
