package plotlyjs.demo.homemade.utils

import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.openmole.plotlyjs.{PlotData, Plotly}
import org.scalajs.dom.html
import plotlyjs.demo.homemade.utils.Utils.ExtraTraceManager.ExtraTracesRef
import plotlyjs.demo.homemade.utils.Vectors._
import plotlyjs.demo.utils.Utils.printCode

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
        //println(name + "...")
        f()
        //println(name + ".")
        busy = false
      } else {
        //println(name + " skipped")
      }
    }
  }

  class ExtraTraceManager(plotDiv: ReactiveHtmlElement[html.Div], initialTraceCount: Int) {

    private var extraSize = 0
    private var refs = Seq[ExtraTracesRef]()

    def addTraces(plotDataSeq: Seq[PlotData]): ExtraTracesRef = {
      refs = refs :+ new ExtraTracesRef(extraSize, extraSize + plotDataSeq.size)

      Plotly.addTraces(plotDiv.ref, plotDataSeq.map(Option(_).orUndefined).toJSArray)
      extraSize = refs.last.to

      refs.last
    }

    def deleteTraces(ref: ExtraTracesRef): Unit = {
      val size = ref.size
      Plotly.deleteTraces(
        plotDiv.ref,
        (ref.from until ref.to)
          .map(_ + initialTraceCount)
          .map(_.toDouble)
          .toJSArray
      )
      extraSize -= size

      refs = refs.filterNot(_ == ref)
      refs
        .filter(ref.to <= _.from)
        .foreach { nextRef =>
          nextRef.from -= size
          nextRef.to -= size
      }
    }

    def deleteAllTraces(): Unit = {
      Plotly.deleteTraces(
        plotDiv.ref,
        (0 until extraSize)
          .map(_ + initialTraceCount)
          .map(_.toDouble)
          .toJSArray
      )
      extraSize = 0
      refs = Seq[ExtraTracesRef]()
    }

    def updateTraces(optionRef: Option[ExtraTracesRef], optionDataSeq: Option[Seq[PlotData]]): Option[ExtraTracesRef] = {
      optionRef.foreach(deleteTraces)
      optionDataSeq.map(addTraces)
    }

  }

  object ExtraTraceManager {

    class ExtraTracesRef(var from: Int, var to: Int) {
      def size: Int = to - from
    }

  }

}
