package plotlyjs.demo.demo

import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.{Color, Plotly}
import org.openmole.plotlyjs.all._
import plotlyjs.demo.directions.RegularDirections
import plotlyjs.demo.utils.PointSet.MIN
import plotlyjs.demo.utils.{Data, PointSet}
import plotlyjs.demo.utils.Vectors.ImplicitVector

import scala.collection.immutable.HashMap
import scala.math.{abs, random}
import scala.scalajs.js.JSConverters.JSRichIterableOnce

object ParallelCoordinatesDemo {

  lazy private val sc = sourcecode.Text {

    val plotDiv = div()

    val dim = 3
    val results = Data.dim8Sample100.map(_.drop(8 - dim))
    val pointSet = new PointSet(results)
      .optimizationProblems(Seq.fill(dim)(MIN))
      .lowerPlotIsBetter

    val directions = RegularDirections.nSphereCovering(dim, Math.PI/4 / 4)
    val groupedResults = pointSet.spaceNormalizedOutputs.groupBy(vector =>
      directions
        .zip(directions.map(direction => abs(vector.dot(direction))))
        .maxBy(_._2)._1
    ).values
    //println(groupedResults.size)
    //groupedResults.foreach(group => println(s"  ${group.size}"))
    //println()

    val plotDataSeq = groupedResults.map(group => {
      parallelCoordinates
        .line(
          line.set(Color.rgb((random() * 255).toInt, (random() * 255).toInt, (random() * 255).toInt))
        )
        .dimensions(group.transpose.map(values => dimension.values(values.toJSArray)._result).toJSArray)
        ._result
    })

    /*
    val directionColors = HashMap.from(directions.zip(directions.map((_, Color.rgb((random() * 255).toInt, (random() * 255).toInt, (random() * 255).toInt)))))
    val resultColors = results.map(vector => directionColors(directions.zip(directions.map(direction => abs(vector.dot(direction)))).maxBy(_._2)._1))

    val plotDataSeq = Seq(
      parallelCoordinates
        .line(line
          .
        )
        .dimensions(results.transpose.map(values => dimension.values(values.toJSArray)._result).toJSArray)
        ._result
    )
    */

    Plotly.newPlot(plotDiv.ref, plotDataSeq.toJSArray)

    plotDiv
  }

  val elementDemo: ElementDemo = new ElementDemo{
    def title: String = "Parallel Coordinates"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
