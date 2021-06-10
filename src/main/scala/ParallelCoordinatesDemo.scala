package plotlyjs.demo

import com.raquo.laminar.api.L._
import plotlyjs.demo.PointSet._

object ParallelCoordinatesDemo {

  private val sc = sourcecode.Text {

    val plotDiv = div()

    val results = Data.dim8Sample100

    val pointSet = new PointSet(results)
      .optimizationProblems(Seq.fill(8)(MIN))
      .higherPlotIsBetter

    //val data = parallelcoordinates //TODO add to plotlyjs
    plotDiv
  }

  val elementDemo: ElementDemo = new ElementDemo{
    def title: String = "Parallel Coordinates"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
