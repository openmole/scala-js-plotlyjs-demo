package plotlyjs.demo.homemade.utils

import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.openmole.plotlyjs.{PlotData, Plotly}
import org.scalajs.dom.html
import plotlyjs.demo.homemade.utils.Vectors._

import scala.math.{ceil, random}
import scala.scalajs.js.JSConverters._

object Utils {

  def randomizeDimensions(seq: Seq[Vector]): Seq[Vector] = {
    seq.headOption.map(head => {
      val dimension = head.dimension
      val mulVector = (() => ceil(10 * random)) at dimension
      val addVector = (() => 10 * random - 5) at dimension
      seq
        .map(_.mul(mulVector))
        .map(_.add(addVector))
    }).getOrElse(Seq[Vector]())
  }

  class SkipOnBusy {
    private var busy = false
    def skipOnBusy(name: String, f: () => Unit): Unit = {
      if(!busy) {
        busy = true
        println(name + "...")
        f()
        println(name + ".")
        busy = false
      } else {
        println(name + " skipped")
      }
    }
  }

  class ExtraTraceManager(plotDiv: ReactiveHtmlElement[html.Div], initialTraceCount: Int) {

    private var extraCount = 0

    def addTraces(plotDataSeq: Seq[PlotData]): Unit = {
      Plotly.addTraces(plotDiv.ref, plotDataSeq.map(Option(_).orUndefined).toJSArray)
      extraCount += plotDataSeq.size
    }

    def deleteTraces(): Unit = {
      Plotly.deleteTraces(plotDiv.ref, (0 until extraCount).map(_ + initialTraceCount).map(_.toDouble).toJSArray)
      extraCount = 0
    }

  }

}
